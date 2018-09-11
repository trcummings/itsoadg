module Game.Util.Texture where

import Data.Word (Word8)
import Foreign.Marshal.Alloc (free)
import System.IO.Error (catchIOError)
import Graphics.Rendering.OpenGL
  ( TextureObject(..)
  , Size(..)
  , PixelData(..)
  , TextureTarget2D     (Texture2D)
  , TextureFilter       (Linear', Nearest)
  , TextureFunction     (Modulate)
  , PixelInternalFormat (RGBA')
  , textureFunction
  , build2DMipmaps
  , textureFilter
  , textureBinding
  , genObjectNames
  , ($=) )

import Game.Util.TGA (readTga)

-- read a list of images and returns a list of textures
-- all images are assumed to be in the TGA image format
getAndCreateTextures :: [FilePath] -> IO [Maybe TextureObject]
getAndCreateTextures fileNames = do
   texData <- mapM readImageC    fileNames
   texObjs <- mapM createTexture texData
   return texObjs

-- read an image file, return a texture
-- images are assumed to be in the TGA image format
getAndCreateTexture :: FilePath -> IO (Maybe TextureObject)
getAndCreateTexture path = do
  texData <- readImageC    path
  texObj  <- createTexture texData
  return texObj

-- read the image data
readImageC :: FilePath -> IO (Maybe (Size, PixelData Word8))
readImageC path = catchIOError (readTga path) $ \_ -> do
  putStrLn $ "Game.Util.Texture: Missing texture at " ++ path
  return Nothing

-- creates the texture
createTexture :: (Maybe (Size, PixelData a)) -> IO (Maybe TextureObject)
createTexture Nothing                                         = return Nothing
createTexture (Just ((Size x y), pixels@(PixelData _ _ ptr))) = do
  -- generate our texture
  [texName] <- genObjectNames 1
  -- make our new texture the current texture
  textureBinding Texture2D $= Just texName
  build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pixels
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  textureFunction $= Modulate
  free ptr
  return (Just texName)
