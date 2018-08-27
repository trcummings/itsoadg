{-# LANGUAGE ScopedTypeVariables #-}

module Game.Util.Render where

import qualified SDL
import           SDL (($=), Point(..))
import qualified Animate
import qualified SDL.Image as Image (load)
import qualified SDL.Raw.Video as Raw (allocFormat, freeFormat, convertSurface)
import qualified SDL.Internal.Numbered as Numbered
import           Foreign.C.Types (CInt)
import           GHC.Int (Int32)
import           Linear (V4(..), V2(..), (^/))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (catMaybes)
import           Data.Coerce (coerce)
import           Paths_itsoadg (getDataFileName)

import           Game.Sprite (rectFromClip)
import           Game.Util.Constants
  ( toPixels
  , spriteSize
  , screenWidth
  , screenHeight )
import           Game.Util.AABB (broadPhaseAABB)
import           Game.Util.TileMap (basicTilemap')
import           Game.System.Player (stepPlayerAnimation)
import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
import           Game.Wrapper.Apecs (Apecs(..))
import           Game.Types
  ( Unit(..)
  , Camera(..)
  , Position(..)
  , Velocity(..)
  , BoundingBox(..)
  , MousePosition(..)
  , Font(..)
  , Jump(..)
  , Texture(..)
  , Player(..), PlayerKey(..)
  , Seconds(..)
  , SpriteSheet(..)
  , Animations(..)
  , FlowMeter(..)
  , AABB(..), dims, center
  , TileType(..)
  , VideoConfig(..) )


toTexture :: SDL.Renderer -> SDL.Surface -> IO Texture
toTexture r surface = do
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  -- once we've made a texture from the surface, free the surface
  SDL.freeSurface surface
  return (Texture t size)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= Image.load
  toTexture r surface

renderTexture :: SDL.Renderer
              -> Texture
              -> Point V2 CInt
              -> Maybe (SDL.Rectangle CInt)
              -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in  SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

makeRect :: V2 CInt -> V2 CInt -> Maybe (SDL.Rectangle CInt)
makeRect pos dims = Just $ SDL.Rectangle (SDL.P pos) dims

vZero = V2 0 0

renderText :: SDL.Renderer -> [(Char, Texture)] -> V2 Unit -> String -> IO ()
renderText renderer f p txt = do
   let textures = catMaybes (map (\c -> lookup c f) txt)
       spacingMap = [xy | xy <- [1..700], xy `mod` 14 == 0]
       textPosMap = zip textures spacingMap
   mapM_ (\(Texture t s, pMod) -> do
     liftIO $ renderTexture
       renderer
       (Texture t s)
       (P $ (toPixels <$> p) + (V2 pMod 0))
       (makeRect vZero s) ) textPosMap

renderSprite :: SDL.Renderer
             -> Animate.SpriteSheet key SDL.Texture Seconds
             -> Animate.SpriteClip  key
             -> V2 Unit
             -> IO ()
renderSprite renderer ss clip pos = do
  let sSheet = Animate.ssImage ss
      SDL.Rectangle (SDL.P clipPos) dims = rectFromClip clip
  renderTexture
    renderer
    (Texture sSheet dims)
    (SDL.P $ toPixels <$> pos)
    (makeRect clipPos dims)

prepNextRender :: SDL.Renderer -> IO ()
prepNextRender renderer = do
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

runRender :: SDL.Renderer -> IO ()
runRender renderer = do
  liftIO $ SDL.present renderer
