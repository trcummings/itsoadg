module Game.Util.BSP.Render where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil           as U
import qualified Linear                    as L
import qualified Data.Array.MArray         as Arr (readArray)
import           Foreign.Marshal (advancePtr)
import           Foreign.Ptr     (Ptr, plusPtr)
import           Control.Monad   (when, unless)
import           Linear          ((!*!))
import           SDL             (($=))

import           Game.Util.BSP.Frustum (Frustum, getFrustum, boxInFrustum)
import           Game.Util.BSP.Indices
import           Game.Util.BSP.BitSet
import           Game.Util.BSP.Util
import           Game.Util.Program     (getUniform)
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
  GL.currentProgram $= Just (_glProgram sProgram)
  -- attribs
  GL.vertexAttribArray (getALoc "vertexPosition") $= GL.Enabled
  GL.vertexAttribArray (getALoc "vertexUV")       $= GL.Enabled
  GL.vertexAttribArray (getALoc "lightmapUV")     $= GL.Enabled
  -- uniforms
  let vp = getUniform sProgram "VP"
      ts = getUniform sProgram "textureSampler"
      ls = getUniform sProgram "lightmapSampler"
  -- transform VP uniform
  (p !*! v) `U.asUniform` vp
  -- bind texture to TextureUnit 0
  -- set "textureSampler" sampler to use Texture Unit 0
  GL.activeTexture $= GL.TextureUnit 0
  GL.uniform ts    $= GL.Index1 (0 :: GL.GLint)
  -- bind texture to TextureUnit 0
  -- set "lightmapSampler" sampler to use Texture Unit 1
  GL.activeTexture $= GL.TextureUnit 1
  GL.uniform ls    $= GL.Index1 (1 :: GL.GLint)

  leaf <- findLeaf cPos $ _tree mapRef
  renderBSP' mats leaf mapRef


-- we have to reset the openGL state after rendering
renderBSPCleanUp :: IO ()
renderBSPCleanUp = do
  GL.vertexAttribArray (getALoc "vertexPosition") $= GL.Disabled
  GL.vertexAttribArray (getALoc "vertexUV")       $= GL.Disabled
  GL.vertexAttribArray (getALoc "lightmapUV")     $= GL.Disabled
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.currentProgram            $= Nothing

getALoc :: String -> GL.AttribLocation
getALoc "vertexPosition" = GL.AttribLocation 0
getALoc "vertexUV"       = GL.AttribLocation 1
getALoc "lightmapUV"     = GL.AttribLocation 2
getALoc _                = error "Bad input in Game.Util.BSP.Render.getALoc"


-- given a position finds in the tree where the position lies in
findLeaf :: L.V3 Float -> Tree -> IO BSPLeaf
findLeaf pos@(L.V3 x y z) (Branch node left right) =
  let (L.V3 px py pz) = realToFrac <$> _planeNormal node
      d               = realToFrac $ _dist node
      dstnc           = (px * x) + (py * y) + (pz * z) - d
      branch          = if dstnc >= 0 then left else right
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
  bsSize  <- sizeBS $ _bitset mp
  newBS   <- emptyBS bsSize
  -- render each leaf
  mapM_ (renderLeaves (getFrustum vp) newBS visFunc mp) (_leaves mp)
  -- clean up after the render
  renderBSPCleanUp
    where visFunc = isClusterVisible (_visData mp) (_cluster leaf)


-- renders a BSP leaf if it is visible
renderLeaves :: Frustum
             -> BitSet
             -> (Int -> IO Bool)
             -> BSPMap
             -> BSPLeaf
             -> IO ()
renderLeaves frustum bitSet func mp leaf  = do
  clusterVisible <- func (_cluster leaf)
  when clusterVisible $ do
    let lMin :: L.V3 Float = realToFrac <$> _leafMin leaf
        lMax :: L.V3 Float = realToFrac <$> _leafMax leaf
    -- AABB in frustum test goes here
    when (boxInFrustum frustum lMin lMax) $ do
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
  isSet <- isSetBS bitSet (_faceNo face)
  unless isSet $ do
    setBS bitSet $ _faceNo face
    let vertData = _vertexData mp
        vIndices = _vindices   mp
        buffers  = _buffers    mp
    case _faceType face of
      1 -> renderPolygonFace buffers face vertData vIndices
      2 -> renderPatches     buffers face
      3 -> renderMeshFace    buffers face vertData vIndices
      -- 4 would be to render a billboard, but we didn't currently
      -- support this. TODO, however
      _ -> pure ()
  renderFaces bitSet mp faces


-- surface rendering --

-- renders a polygon surface
renderPolygonFace :: BSPBuffers
                  -> BSPFace
                  -> VertexArrays
                  -> Ptr GL.GLint
                  -> IO ()
renderPolygonFace bufs face _ _ =  do
  -- putStrLn "===============renderPolygonFace==============="
  let (a, b, c, d) = _arrayPtrs face
  GL.bindBuffer GL.ArrayBuffer  $= Just (_bspPosition bufs)
  -- printGLErrors "drawPolygonFace buffer position"
  GL.vertexAttribPointer (getALoc "vertexPosition") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 a )
  -- printGLErrors "drawPolygonFace position pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspTexCoords bufs)
  -- printGLErrors "drawPolygonFace buffer texture"
  GL.activeTexture $= GL.TextureUnit 0
  -- printGLErrors "drawPolygonFace active texture"
  GL.textureBinding GL.Texture2D $= _textureObj face
  -- printGLErrors "drawPolygonFace binding texture"
  GL.vertexAttribPointer (getALoc "vertexUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 b )
  -- printGLErrors "drawPolygonFace texture pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspLmpCoords bufs)
  -- printGLErrors "drawPolygonFace buffer lightmap"
  GL.activeTexture $= GL.TextureUnit 1
  -- printGLErrors "drawPolygonFace active lightmap"
  GL.textureBinding GL.Texture2D $= _lightmapObj face
  -- printGLErrors "drawPolygonFace binding lightmap"
  GL.vertexAttribPointer (getALoc "lightmapUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 c )
  -- printGLErrors "drawPolygonFace lightmap pointer"

  -- GL.drawRangeElements
  --   GL.Triangles
  --   (0, _numOfIndices face)
  --   (_numOfIndices face)
  --   GL.UnsignedInt
  --   d
  -- -- GL.drawElements GL.Triangles (_numOfIndices face) GL.UnsignedInt d
  GL.drawArrays GL.Triangles 0 (_numOfIndices face)
  -- printGLErrors "drawPolygonFace draw"
  -- putStrLn "===============renderPolygonFace==============="


-- renders a mesh face
renderMeshFace :: BSPBuffers
               -> BSPFace
               -> VertexArrays
               -> Ptr GL.GLint
               -> IO ()
renderMeshFace bufs face vertexArrays vIndex = do
  putStrLn "===============renderMeshFace==============="
  let (vertexPtr, texturePtr, lightmapPtr, _, _) = vertexArrays
      startVIndex                                = _startVertIndex face

  GL.bindBuffer GL.ArrayBuffer  $= Just (_bspPosition bufs)
  printGLErrors "renderMeshFace buffer position"
  GL.vertexAttribPointer (getALoc "vertexPosition") $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 3 GL.Float 0 (plusPtr vertexPtr (12 * startVIndex)) )
  printGLErrors "renderMeshFace position pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspTexCoords bufs)
  printGLErrors "renderMeshFace buffer texture"
  GL.activeTexture               $= GL.TextureUnit 0
  printGLErrors "renderMeshFace active texture"
  GL.textureBinding GL.Texture2D $= _textureObj face
  printGLErrors "renderMeshFace binding texture"
  GL.vertexAttribPointer (getALoc "vertexUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (advancePtr texturePtr (2 * startVIndex)) )
  printGLErrors "renderMeshFace texture pointer"

  GL.bindBuffer GL.ArrayBuffer $= Just (_bspLmpCoords bufs)
  printGLErrors "renderMeshFace buffer lightmap"
  GL.activeTexture               $= GL.TextureUnit 1
  printGLErrors "renderMeshFace active lightmap"
  GL.textureBinding GL.Texture2D $= _lightmapObj face
  printGLErrors "renderMeshFace binding lightmap"
  GL.vertexAttribPointer (getALoc "lightmapUV") $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (plusPtr lightmapPtr (8 * startVIndex)) )
  printGLErrors "renderMeshFace lightmap pointer"

  GL.drawArrays GL.Triangles 0 (_numOfIndices face)
  printGLErrors "renderMeshFace draw"
  putStrLn "===============renderMeshFace==============="

-- renders patch surfaces
renderPatches :: BSPBuffers -> BSPFace -> IO ()
renderPatches bufs face = mapM_ (renderPatch bufs face) (_patch face)

renderPatch :: BSPBuffers -> BSPFace -> BSPPatch -> IO ()
renderPatch bufs face bsppatch = do
  putStrLn "===============renderPatch==============="
  let patchPtr = _patchPtr bsppatch -- pointer to patch vertices
  -- GL.arrayPointer GL.VertexArray $=
  --   -- stride of 28 -- why?
  --   GL.VertexArrayDescriptor 3 GL.Float 28 patchPtr
  -- GL.clientState GL.VertexArray $= GL.Enabled

  GL.activeTexture               $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= _textureObj face
  -- GL.clientActiveTexture $= GL.TextureUnit 0
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   -- 12 pointers ahead of the patch pointer -- why?
  --   -- brushes have size of 12
  --   GL.VertexArrayDescriptor 2 GL.Float 28 (plusPtr patchPtr 12) -- maybe its a pointer to "a"
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled

  GL.activeTexture               $= GL.TextureUnit 1
  GL.textureBinding GL.Texture2D $= _lightmapObj face
  -- GL.clientActiveTexture $= GL.TextureUnit 1
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   -- 20 pointers ahead of the patch pointer -- why?
  --   -- brush sides have total size of 8
  --   GL.VertexArrayDescriptor 2 GL.Float 28 (plusPtr patchPtr 20) -- pointer to "b"? (12 + 8)
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled

  GL.multiDrawElements
    GL.TriangleStrip -- we're drawing a "curved surface" here
    (_numIndexPtr bsppatch) -- number of indices
    GL.UnsignedInt
    (_indexPtrPtr bsppatch) -- pointer to indices
    (fromIntegral (_patchLOD bsppatch)) -- level of tesselation
  putStrLn "===============renderPatch==============="
