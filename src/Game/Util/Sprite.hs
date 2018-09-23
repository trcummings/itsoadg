module Game.Util.Sprite where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Animate                   as A
import           System.FilePath      ((</>))

import           Game.Loaders.Texture (getAndCreateTexture)
import           Game.Util.Constants  (texturePath)
import           Game.Types
  ( Texture(..)
  , Seconds(..)
  , SheetInfo )

loadTextureNoColor :: FilePath -> Maybe A.Color -> IO Texture
loadTextureNoColor path _ = getAndCreateTexture (texturePath </> path)

loadSpriteSheet :: FilePath -> IO SheetInfo
loadSpriteSheet path = A.readSpriteSheetJSON loadTextureNoColor path
