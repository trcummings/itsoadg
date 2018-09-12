module Game.Util.BSP.Render where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import qualified Data.Array.MArray         as Arr (readArray)
import           SDL             (($=))
import           Foreign.Marshal (advancePtr)
import           Foreign.Ptr     (Ptr, plusPtr)
import           Control.Monad   (when, unless)

import           Game.Util.BSP.Frustum (Frustum, getFrustum, boxInFrustum)
import           Game.Util.BSP.Indices
import           Game.Util.BSP.BitSet
import           Game.Util.BSP.Util
import           Game.Util.GLError     (printGLErrors)
import           Game.Types.BSP
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..) )


--BSP rendering
renderBSP :: (ProjectionMatrix, ViewMatrix)
          -> Position3D
          -> BSPMap
          -> IO ()
renderBSP mats (Position3D cPos) mapRef = do
  -- GL.activeTexture $= GL.TextureUnit 0
  -- GL.clientActiveTexture $= GL.TextureUnit 0
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled
  --
  -- GL.activeTexture $= GL.TextureUnit 1
  -- GL.clientActiveTexture $= GL.TextureUnit 1
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled

  leaf <- findLeaf cPos $ _tree mapRef
  renderBSP' mats leaf mapRef


-- we have to reset the openGL state after rendering
renderBSPCleanUp :: IO ()
renderBSPCleanUp = do
  return ()
  -- let buf = _buffers mapRef
  -- -- deactivate all attrib buffers
  -- GL.vertexAttribArray $ _bspPositions      buf $= GL.Disabled
  -- GL.vertexAttribArray $ _bspTexCoords      buf $= GL.Disabled
  -- GL.vertexAttribArray $ _bsplightmapCoords buf $= GL.Disabled
  -- GL.vertexAttribArray $ _bspNormals        buf $= GL.Disabled
  -- GL.vertexAttribArray $ _bspRGBAVals       buf $= GL.Disabled

  -- GL.activeTexture $= GL.TextureUnit 1
  -- GL.clientState GL.TextureCoordArray $= GL.Disabled
  -- GL.texture GL.Texture2D $= GL.Disabled
  -- GL.activeTexture $= GL.TextureUnit 0
  -- GL.clientActiveTexture $= GL.TextureUnit 0
  -- GL.clientState GL.TextureCoordArray $= GL.Disabled
  -- GL.texture GL.Texture2D $= GL.Disabled


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
    case _faceType face of
      1 -> renderPolygonFace face vertData vIndices
      2 -> renderPatches     face
      3 -> renderMeshFace    face vertData vIndices
      -- 4 would be to render a billboard, but we didn't currently
      -- support this. TODO, however
      _ -> pure ()
  renderFaces bitSet mp faces


-- surface rendering --

-- renders a polygon surface
renderPolygonFace :: BSPFace
                  -> VertexArrays
                  -> Ptr GL.GLint
                  -> IO ()
renderPolygonFace face _ _ =  do
  let (a, b, c, d) = _arrayPtrs face
  -- GL.arrayPointer GL.VertexArray $=
  --   GL.VertexArrayDescriptor 3 GL.Float 0 a
  -- GL.clientState GL.VertexArray $= GL.Enabled

  GL.activeTexture $= GL.TextureUnit 0
  -- GL.clientActiveTexture $= GL.TextureUnit 0
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   GL.VertexArrayDescriptor 2 GL.Float 0 b
  GL.textureBinding GL.Texture2D $= _textureObj face

  GL.activeTexture $= GL.TextureUnit 1
  -- GL.clientActiveTexture $= GL.TextureUnit 1
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   GL.VertexArrayDescriptor 2 GL.Float 0 c
  GL.textureBinding GL.Texture2D $= _lightmapObj face

  GL.drawRangeElements
    GL.Triangles
    (0, _numOfIndices face)
    (_numOfIndices face)
    GL.UnsignedInt
    d
  -- -- GL.drawElements GL.Triangles (_numOfIndices face) GL.UnsignedInt d


-- renders a mesh face
renderMeshFace :: BSPFace
               -> VertexArrays
               -> Ptr GL.GLint
               -> IO ()
renderMeshFace face vertexArrays vIndex = do
  let (vertexPtr, texturePtr, lightmapPtr, _, _) = vertexArrays
      startVIndex                                = _startVertIndex face
  -- GL.arrayPointer GL.VertexArray $=
  --   GL.VertexArrayDescriptor 3 GL.Float 0
  --     (plusPtr vertexPtr (12 * startVIndex))
  -- GL.clientState GL.VertexArray  $= GL.Enabled

  GL.activeTexture $= GL.TextureUnit 0
  -- GL.clientActiveTexture $= GL.TextureUnit 0
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   GL.VertexArrayDescriptor 2 GL.Float 0
  --     (advancePtr texturePtr (2 * startVIndex))
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= _textureObj face

  GL.activeTexture $= GL.TextureUnit 1
  -- GL.clientActiveTexture $= GL.TextureUnit 1
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   GL.VertexArrayDescriptor 2 GL.Float 0
  --     (plusPtr lightmapPtr (8 * startVIndex))
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= _lightmapObj face

  GL.drawRangeElements
    GL.Triangles
    (0, fromIntegral (_numOfVerts face))
    (_numOfIndices face)
    GL.UnsignedInt
    (plusPtr vIndex (4 * _startIndex face))
  -- -- GL.drawElements GL.Triangles (_numOfIndices face) GL.UnsignedInt d

-- renders patch surfaces
renderPatches :: BSPFace -> IO ()
renderPatches face = mapM_ (renderPatch face) (_patch face)

renderPatch :: BSPFace -> BSPPatch -> IO ()
renderPatch face bsppatch = do
  let patchPtr = _patchPtr bsppatch -- pointer to patch vertices
  -- GL.arrayPointer GL.VertexArray $=
  --   -- stride of 28 -- why?
  --   GL.VertexArrayDescriptor 3 GL.Float 28 patchPtr
  -- GL.clientState GL.VertexArray $= GL.Enabled

  GL.activeTexture $= GL.TextureUnit 0
  -- GL.clientActiveTexture $= GL.TextureUnit 0
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   -- 12 pointers ahead of the patch pointer -- why?
  --   -- brushes have size of 12
  --   GL.VertexArrayDescriptor 2 GL.Float 28 (plusPtr patchPtr 12) -- maybe its a pointer to "a"
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= _textureObj face

  GL.activeTexture $= GL.TextureUnit 1
  -- GL.clientActiveTexture $= GL.TextureUnit 1
  -- GL.arrayPointer GL.TextureCoordArray $=
  --   -- 20 pointers ahead of the patch pointer -- why?
  --   -- brush sides have total size of 8
  --   GL.VertexArrayDescriptor 2 GL.Float 28 (plusPtr patchPtr 20) -- pointer to "b"? (12 + 8)
  -- GL.clientState GL.TextureCoordArray $= GL.Enabled
  -- GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= _lightmapObj face

  GL.multiDrawElements
    GL.TriangleStrip -- we're drawing a "curved surface" here
    (_numIndexPtr bsppatch) -- number of indices
    GL.UnsignedInt
    (_indexPtrPtr bsppatch) -- pointer to indices
    (fromIntegral (_patchLOD bsppatch)) -- level of tesselation
