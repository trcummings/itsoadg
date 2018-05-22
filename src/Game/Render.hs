module Game.Render where

import qualified SDL
import           SDL (($=), Point(..))
import           Foreign.C.Types (CInt)
import           Linear (V4(..), V2(..))
import           Apecs (cmapM_, cmap)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (catMaybes)

import           Game.World (System')
import           Game.Constants
  ( Unit(..)
  , toPixels
  , spriteSize )
import           Game.Types
  ( Player(..)
  , Position(..)
  , Velocity(..)
  , BoundingBox(..)
  , Font(..)
  , Texture(..) )

import Paths_itsoadg (getDataFileName)

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
  surface <- getDataFileName filePath >>= SDL.loadBMP
  toTexture r surface

renderTexture :: SDL.Renderer
              -> Texture
              -> Point V2 CInt
              -> Maybe (SDL.Rectangle CInt)
              -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

stepRender :: SDL.Renderer -> System' ()
stepRender renderer = do
  -- render "player"
  cmapM_ $ \(Player, Position p, Velocity v, Texture t s) -> do
    liftIO $ renderTexture
      renderer
      (Texture t s)
      (P $ toPixels <$> p)
      (Just $ SDL.Rectangle (P (V2 0 0)) (toPixels <$> spriteSize))

  -- render small font
  cmapM_ $ \(Font f, Position p) -> do
    cmapM_ $ \(Player, Position pp, Velocity pv) -> do
      let pText = "Player: "
            ++ (show $ toPixels <$> pp)
            ++ ", "
            ++ (show $ toPixels <$> pv)
          textures = catMaybes (map (\c -> lookup c f) pText)
          spacingMap = [xy | xy <- [1..700], xy `mod` 14 == 0]
          textPosMap = zip textures spacingMap
      mapM_ (\(Texture t s, pMod) -> do
        liftIO $ renderTexture
          renderer
          (Texture t s)
          (P $ (toPixels <$> p) + (V2 pMod 0))
          (Just $ SDL.Rectangle (P (V2 0 0)) s)
            ) (take 40 textPosMap)

  -- render bounding boxes
  cmapM_ $ \(BoundingBox (V2 w h), Position (V2 x y)) -> do
    liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 maxBound maxBound
    liftIO $ SDL.drawRect
      renderer
      (Just $ SDL.Rectangle (P (toPixels <$> V2 x y)) (toPixels <$> V2 w h))

prepNextRender :: SDL.Renderer -> IO ()
prepNextRender renderer = do
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

runRender :: SDL.Renderer -> IO ()
runRender renderer = do
  liftIO $ SDL.present renderer
