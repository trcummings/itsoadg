{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Sprite where

import qualified Animate
import qualified SDL
import qualified SDL.Raw as Raw (freeFormat, allocFormat, convertSurface)
import qualified SDL.Image as Image (load)
import qualified SDL.Internal.Numbered as Numbered (toNumber)
import           SDL (($=))
import           Foreign.C.Types (CInt)
import           Linear (V2(..), V4(..))
import           Data.Text
import           Control.Monad.IO.Class (liftIO)

import           Game.Util.Constants
  ( frameDeltaSeconds
  , toPixels )
-- import           Game.Render (renderTexture)
import           Game.Types (Texture(..), Unit(..), Seconds(..))


rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{ scX, scY, scW, scH }
  = SDL.Rectangle point dims
    where
      point = SDL.P $ fromIntegral <$> (V2 scX scY)
      dims  = fromIntegral <$> (V2 scW scH)

-- | Produce a new 'SDL.Surface' based on an existing one, but
-- optimized for blitting to the specified 'SDL.PixelFormat'.
convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface0 <- Image.load path
  surface <- convertSurface surface0 SDL.RGBA8888
  SDL.freeSurface surface0
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

-- renderSprite :: SDL.Renderer
--              -> Animate.SpriteSheet key SDL.Texture Seconds
--              -> Animate.SpriteClip  key
--              -> V2 Unit -> IO ()
-- renderSprite renderer ss clip pos = do
--   let sSheet = Animate.ssImage ss
--       clip'@(SDL.Rectangle _ dim) = rectFromClip clip
--   renderTexture
--     renderer
--     (Texture sSheet dim)
--     (SDL.P $ toPixels <$> pos)
--     (Just $ SDL.Rectangle (SDL.P $ toPixels <$> pos) dim)

loadSpriteSheet :: (Animate.KeyName key, Ord key, Bounded key, Enum key)
                => SDL.Renderer
                -> FilePath
                -> IO (Animate.SpriteSheet key SDL.Texture Seconds)
loadSpriteSheet renderer path = do
  Animate.readSpriteSheetJSON loadTexture path
  where
    loadTexture path' c = SDL.createTextureFromSurface renderer =<< loadSurface path' c
