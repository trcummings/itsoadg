module Game.Util.Shader.Buffer (makeBuffer, replaceBuffer) where

import qualified Graphics.Rendering.OpenGL as GL
import           Foreign.Marshal.Array (withArray, withArrayLen)
import           Foreign.Storable      (Storable, sizeOf)
import           SDL                   (($=))


makeBuffer :: forall a. Storable a
           => GL.BufferTarget -> [a] -> IO GL.BufferObject
makeBuffer target elems = do
  arrayBuffer <- GL.genObjectName

  GL.bindBuffer target $= Just arrayBuffer
  withArrayLen elems $ \len ptr ->
    let n = fromIntegral $ len * sizeOf (error "makeBuffer" :: a)
    in GL.bufferData target $= (n, ptr, GL.StaticDraw)
  return arrayBuffer

replaceBuffer :: forall a. Storable a
              => GL.BufferTarget -> [a] -> Int -> IO ()
replaceBuffer target elems len =
  withArray elems $ \ptr -> do
    let n = fromIntegral $ len * sizeOf (error "replaceBuffer" :: a)
    GL.bufferData target $= (n, ptr, GL.StaticDraw)
