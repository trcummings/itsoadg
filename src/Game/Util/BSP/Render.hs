module Game.Util.BSP.Render where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil           as U
import qualified Linear                    as L
import qualified Data.Array.MArray         as Arr (readArray)
import           Data.Array      ((!))
import           Foreign.Marshal (advancePtr)
import           Foreign.Ptr     (Ptr, plusPtr)
import           Control.Monad   (when, unless, (<=<))
import           Linear          ((!*!))
import           SDL             (($=))

import           Game.Util.BSP.Frustum (Frustum, getFrustum, boxInFrustum)
import           Game.Util.BSP.Indices
import           Game.Util.BSP.BitSet
import           Game.Util.BSP.Util
import           Game.Loaders.Program     (getUniform)
import           Game.Util.GLError     (printGLErrors)
import           Game.Types.BSP
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , ShaderProgram(..) )

type BSPRenderData = (BSPMap, ShaderProgram)

--BSP rendering
renderBSP :: (ProjectionMatrix, ViewMatrix)
          -> Position3D
          -> BSPRenderData
          -> IO ()
renderBSP mats@(ProjectionMatrix p, ViewMatrix v)
          (Position3D cPos)
          (mapRef, sProgram) = do
  -- set up for the render
  GL.currentProgram                   $= Just (_glProgram sProgram)
  printGLErrors "renderBSP set program"
  GL.bindBuffer GL.ElementArrayBuffer $= Just (_bspIndices . _buffers $ mapRef)
  printGLErrors "renderBSP bind elem buffer"
  -- attribs
  GL.vertexAttribArray (getALoc "vertexPosition") $= GL.Enabled
  GL.vertexAttribArray (getALoc "textureUV")      $= GL.Enabled
  GL.vertexAttribArray (getALoc "lightmapUV")     $= GL.Enabled
  printGLErrors "renderBSP enable attribs"
  -- uniforms
  let view = getUniform sProgram "view"
      proj = getUniform sProgram "proj"
      ts = getUniform sProgram "textureSampler"
      ls = getUniform sProgram "lightmapSampler"
  -- transform VP uniform
  v `U.asUniform` view
  p `U.asUniform` proj
  printGLErrors "renderBSP set VP uniform"
  -- bind texture to TextureUnit 0
  -- set "textureSampler" sampler to use Texture Unit 0
  GL.activeTexture $= GL.TextureUnit 0
  GL.uniform ts    $= GL.Index1 (0 :: GL.GLint)
  -- bind texture to TextureUnit 0
  -- set "lightmapSampler" sampler to use Texture Unit 1
  GL.activeTexture $= GL.TextureUnit 1
  GL.uniform ls    $= GL.Index1 (1 :: GL.GLint)

  -- find the leaf closest to the given position (in this case, the
  -- camera position) in the BSP & start from there
  leaf <- findLeaf cPos $ _tree mapRef
  renderBSP' mats leaf mapRef


-- we have to reset the openGL state after rendering
renderBSPCleanUp :: IO ()
renderBSPCleanUp = do
  GL.vertexAttribArray (getALoc "vertexPosition") $= GL.Disabled
  GL.vertexAttribArray (getALoc "textureUV")      $= GL.Disabled
  GL.vertexAttribArray (getALoc "lightmapUV")     $= GL.Disabled
  printGLErrors "renderBSPCleanUp disable attribs"
  GL.bindBuffer GL.ArrayBuffer        $= Nothing
  printGLErrors "renderBSPCleanUp unset array buffer"
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  printGLErrors "renderBSPCleanUp unset element buffer"
  GL.currentProgram                   $= Nothing
  printGLErrors "renderBSPCleanUp unset program"

getALoc :: String -> GL.AttribLocation
getALoc "vertexPosition" = GL.AttribLocation 0
getALoc "textureUV"      = GL.AttribLocation 1
getALoc "lightmapUV"     = GL.AttribLocation 2
getALoc _                = error "Bad input in Game.Util.BSP.Render.getALoc"


-- given a position, find in the tree where the position lies in
findLeaf :: L.V3 Float -> Tree -> IO BSPLeaf
findLeaf pos@(L.V3 x y z) (Branch node left right) =
  let (L.V3 px py pz) = realToFrac <$> _planeNormal node
      nodeDist        = realToFrac $ _dist node
      distance        = (px * x) + (py * y) + (pz * z) - nodeDist
      branch          = if distance >= 0 then left else right
  in findLeaf pos branch
findLeaf (L.V3 _ _ _) (Leaf leaf) = return leaf


-- we are actually going across all the leaves in the tree
-- instead of walking the tree and pushing the leaves that
-- we want to render into a stack
renderBSP' :: (ProjectionMatrix, ViewMatrix)
           -> BSPLeaf
           -> BSPMap
           -> IO ()
renderBSP' vp leaf mp = do
  -- create new bitset to traverse the BSP with
  newBS  <- emptyBS <=< sizeBS $ _bitset mp
  let frustum = getFrustum vp
  -- render each leaf
  mapM_ (renderLeaves frustum newBS visFunc mp) (_leaves mp)
  -- clean up after the render
  renderBSPCleanUp
    -- curry in the visData of the BSP, & the cluster data of
    -- the current leaf
    where visFunc = isClusterVisible (_visData mp) (_cluster leaf)


-- renders a BSP leaf if it is visible
renderLeaves :: Frustum
             -> BitSet
             -> (Int -> IO Bool)
             -> BSPMap
             -> BSPLeaf
             -> IO ()
renderLeaves frustum bitSet visFunc mp leaf = do
  -- run visibility test function
  clusterVisible <- visFunc (_cluster leaf)
  when clusterVisible $ do
    let lMin :: L.V3 Float = realToFrac <$> _leafMin leaf
        lMax :: L.V3 Float = realToFrac <$> _leafMax leaf
    -- test frustum against AABB of leaf min & max boundaries
    when (boxInFrustum frustum lMin lMax) $ do
      -- if passes test, render faces of current leaf
      renderFaces bitSet mp (_leafFaces leaf)


-- is an object visible
isObjectVisible :: BSPMap -> L.V3 Float -> L.V3 Float -> IO Bool
isObjectVisible bsp cPos oPos = do
  currentLeaf <- findLeaf cPos (_tree bsp)
  objectLeaf  <- findLeaf oPos (_tree bsp)
  isClusterVisible
    (_visData bsp)
    (_cluster currentLeaf)
    (_cluster objectLeaf)

isClusterVisible :: Maybe BSPVisData -> Int -> Int -> IO Bool
isClusterVisible (Just visdata) current target
  | current < 0 = return True
  | target  < 0 = return False
  | otherwise   =
      Arr.readArray
        (_bitSets visdata)
        ((_bytesPerCluster visdata * current * 8) + target)
isClusterVisible _ _ _ = return False

-- decide how to render face
renderFaces :: BitSet -> BSPMap -> [BSPFace] -> IO ()
renderFaces _ _ [] = return ()
renderFaces bitSet mp (face : faces) = do
  -- have we already set this face in the bitset?
  -- if the face has already been rendered, we know we don't need to
  -- render it again
  isSet <- isSetBS bitSet (_faceNo face)
  unless isSet $ do
    -- set this face in the bitset
    setBS bitSet $ _faceNo face
    case _faceType face of
      1 -> renderPolygonFace mp face
      2 -> renderPatches     mp face
      3 -> renderMeshFace    mp face
      -- 4 would be to render a billboard, but we didn't currently
      -- support this. TODO, however
      _ -> pure ()
  renderFaces bitSet mp faces


-- surface rendering --

-- renders a polygon surface
renderPolygonFace :: BSPMap -> BSPFace -> IO ()
renderPolygonFace mp face =  do
  let (a, b, c, d) = _arrayPtrs face
      buffers      = _buffers mp

  GL.bindBuffer GL.ArrayBuffer  $= Just (_bspPosition buffers)
  printGLErrors "drawPolygonFace buffer position"
  GL.vertexAttribPointer (getALoc "vertexPosition") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 a )
  printGLErrors "drawPolygonFace position pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspTexCoords buffers)
  printGLErrors "drawPolygonFace buffer texture"
  GL.activeTexture $= GL.TextureUnit 0
  printGLErrors "drawPolygonFace active texture"
  GL.textureBinding GL.Texture2D $= _textureObj face
  printGLErrors "drawPolygonFace binding texture"
  GL.vertexAttribPointer (getALoc "textureUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 b )
  printGLErrors "drawPolygonFace texture pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspLmpCoords buffers)
  printGLErrors "drawPolygonFace buffer lightmap"
  GL.activeTexture $= GL.TextureUnit 1
  printGLErrors "drawPolygonFace active lightmap"
  GL.textureBinding GL.Texture2D $= _lightmapObj face
  printGLErrors "drawPolygonFace binding lightmap"
  GL.vertexAttribPointer (getALoc "lightmapUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 c )
  printGLErrors "drawPolygonFace lightmap pointer"

  GL.bindBuffer GL.ElementArrayBuffer $= Just (_bspIndices buffers)
  printGLErrors "drawPolygonFace bind elem buffer"

  GL.drawRangeElements
    GL.Triangles
    (0, _numOfIndices face)
    (_numOfIndices face)
    GL.UnsignedInt
    d
  printGLErrors "drawPolygonFace draw"


-- renders a mesh face
renderMeshFace :: BSPMap -> BSPFace -> IO ()
renderMeshFace mp face = do
  let buffers                  = _buffers mp
      (vPtr, tPtr, lPtr, _, _) = _vertexData mp
      vIdx                     = _vIndices mp
      svIdx                    = _startVertIndex face

  GL.bindBuffer GL.ArrayBuffer  $= Just (_bspPosition buffers)
  printGLErrors "renderMeshFace buffer position"
  GL.vertexAttribPointer (getALoc "vertexPosition") $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 3 GL.Float 0 (plusPtr vPtr (12 * svIdx)) )
  printGLErrors "renderMeshFace position pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspTexCoords buffers)
  printGLErrors "renderMeshFace buffer texture"
  GL.activeTexture               $= GL.TextureUnit 0
  printGLErrors "renderMeshFace active texture"
  GL.textureBinding GL.Texture2D $= _textureObj face
  printGLErrors "renderMeshFace binding texture"
  GL.vertexAttribPointer (getALoc "textureUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (plusPtr tPtr (2 * svIdx)) )
  printGLErrors "renderMeshFace texture pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspLmpCoords buffers)
  printGLErrors "renderMeshFace buffer lightmap"
  GL.activeTexture               $= GL.TextureUnit 1
  printGLErrors "renderMeshFace active lightmap"
  GL.textureBinding GL.Texture2D $= _lightmapObj face
  printGLErrors "renderMeshFace binding lightmap"
  GL.vertexAttribPointer (getALoc "lightmapUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (plusPtr lPtr (8 * svIdx)) )
  printGLErrors "renderMeshFace lightmap pointer"

  GL.drawRangeElements
    GL.Triangles
    (0, _numOfVerts face)
    (_numOfIndices face)
    GL.UnsignedInt
    (plusPtr vIdx (4 * _startIndex face))
  printGLErrors "renderMeshFace draw"

-- renders patch surfaces
renderPatches :: BSPMap -> BSPFace -> IO ()
renderPatches mp face = mapM_ (renderPatch mp face) (_patch face)

renderPatch :: BSPMap -> BSPFace -> BSPPatch -> IO ()
renderPatch mp face patch = do
  let patchPtr = _patchPtr patch -- pointer to patch vertices
      buffers  = _buffers mp

  GL.bindBuffer GL.ArrayBuffer  $= Just (_bspPosition buffers)
  printGLErrors "renderPatch buffer position"
  GL.vertexAttribPointer (getALoc "vertexPosition") $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 3 GL.Float 28 patchPtr )
  printGLErrors "renderPatch position pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspTexCoords buffers)
  printGLErrors "renderPatch buffer texture"
  GL.activeTexture               $= GL.TextureUnit 0
  printGLErrors "renderPatch active texture"
  GL.textureBinding GL.Texture2D $= _textureObj face
  printGLErrors "renderPatch binding texture"
  GL.vertexAttribPointer (getALoc "textureUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 28 (plusPtr patchPtr 12) )
  printGLErrors "renderPatch texture pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspLmpCoords buffers)
  printGLErrors "renderPatch buffer lightmap"
  GL.activeTexture               $= GL.TextureUnit 1
  printGLErrors "renderPatch active lightmap"
  GL.textureBinding GL.Texture2D $= _lightmapObj face
  printGLErrors "renderPatch binding lightmap"
  GL.vertexAttribPointer (getALoc "lightmapUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 28 (plusPtr patchPtr 20) )
  printGLErrors "renderPatch lightmap pointer"

  GL.multiDrawElements
    -- we're drawing a "curved surface" here
    GL.TriangleStrip
    (_numIndexPtr patch)
    GL.UnsignedInt
    (_indexPtrPtr patch)
    -- level of tesselation
    (fromIntegral (_patchLOD patch))
  printGLErrors "renderPatch draw"
