module Game.Loaders.Texture where

import qualified Graphics.Rendering.OpenGL as GL
import           SDL                   (($=))
import           Data.Word             (Word8)
import           Foreign.Marshal.Alloc (free)
import           System.IO.Error       (catchIOError)
import           Control.Monad         ((<=<))

import           Game.Loaders.TGA     (readTga)
import           Game.Util.GLError    (printGLErrors)
import           Game.Types           (Texture(..))

-- read a list of images and returns a list of textures
-- all images are assumed to be in the TGA image format
getAndCreateTextures :: [FilePath] -> IO [Texture]
getAndCreateTextures = mapM getAndCreateTexture

-- read an image file, return a texture
-- images are assumed to be in the TGA image format
getAndCreateTexture :: FilePath -> IO Texture
getAndCreateTexture = createTexture <=< readImageC

-- read the image data
readImageC :: FilePath -> IO (Maybe (GL.TextureSize2D, GL.PixelData Word8))
readImageC path = catchIOError (readTga path) $ \_ -> do
  putStrLn $ "Game.Loaders.Texture: Missing texture at " ++ path
  return Nothing

-- creates the texture
createTexture :: (Maybe (GL.TextureSize2D, GL.PixelData a))
              -> IO Texture
createTexture Nothing =
  return Texture { _textureId   = Nothing
                 , _textureSize = GL.TextureSize2D 0 0 }
createTexture (Just (texSize, pixels@(GL.PixelData _ _ ptr))) = do
  -- generate our texture
  [texName] <- GL.genObjectNames 1
  -- make our new texture the current texture
  GL.textureBinding GL.Texture2D $= Just texName
  printGLErrors "createTexture binding texture"

  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
  printGLErrors "createTexture set texture wrap mode"

  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA' texSize 0 pixels
  printGLErrors "createTexture create texture image 2D"

  GL.generateMipmap' GL.Texture2D
  printGLErrors "createTexture generate mipmaps"

  -- filtering between mipmaps. set to "nearest" for both to maximize
  -- pixelization
  GL.textureFilter  GL.Texture2D $= ((GL.Nearest, Just GL.Nearest), GL.Nearest)
  printGLErrors "createTexture setting texture filter"
  -- textureFunction $= Modulate
  -- printGLErrors "createTexture setting texture function"
  free ptr
  return $ Texture { _textureId   = Just texName
                   , _textureSize = texSize }
