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
import           Game.Collision
  ( AABB(..), dims, center
  , broadPhaseAABB )
import           Game.Types
  ( Player(..)
  , Camera(..)
  , Position(..)
  , Velocity(..)
  , BoundingBox(..)
  , Font(..)
  , Jump(..)
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
  in  SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

renderText :: SDL.Renderer -> [(Char, Texture)] -> V2 Unit -> String -> System' ()
renderText renderer f p txt = do
   let textures = catMaybes (map (\c -> lookup c f) txt)
       spacingMap = [xy | xy <- [1..700], xy `mod` 14 == 0]
       textPosMap = zip textures spacingMap
   mapM_ (\(Texture t s, pMod) -> do
     liftIO $ renderTexture
       renderer
       (Texture t s)
       (P $ (toPixels <$> p) + (V2 pMod 0))
       (Just $ SDL.Rectangle (P (V2 0 0)) s)
         ) textPosMap

stepRender :: SDL.Renderer -> System' ()
stepRender renderer = do
  -- get camera position
  cmapM_ $ \(Camera _ _, Position cameraPos) -> do
    -- render "player"
    cmapM_ $ \(Player, Position p, Velocity v, Texture t s) -> do
      liftIO $ renderTexture
        renderer
        (Texture t s)
        (P $ toPixels <$> (p - cameraPos))
        (Just $ SDL.Rectangle (P (V2 0 0)) (toPixels <$> spriteSize))
    -- render bounding boxes
    cmapM_ $ \(BoundingBox bb, Position p) -> do
      liftIO $ SDL.rendererDrawColor renderer $= V4 255 0 maxBound maxBound
      liftIO $ SDL.drawRect
        renderer
        (Just $ SDL.Rectangle (P (toPixels <$> (p - cameraPos))) (toPixels <$> bb))
    -- render broad phase bounding box
    cmapM_ $ \(bb@(BoundingBox _), p@(Position _), v@(Velocity _)) -> do
      let aabb = broadPhaseAABB bb p v
      liftIO $ SDL.rendererDrawColor renderer $= V4 maxBound maxBound 0 255
      liftIO $ SDL.drawRect
        renderer
        (Just $ SDL.Rectangle (P (toPixels <$> ((center aabb) - cameraPos))) (toPixels <$> (dims aabb)))

  -- render small font
  cmapM_ $ \(Font f, Position p) -> do
    cmapM_ $ \(Player, (Jump jcr ij), Position pp, Velocity pv) -> do
      let pText = "Player: "
            ++ (show $ toPixels <$> pp)
            ++ ", "
            ++ (show $ toPixels <$> pv)
          jText = "Jump: " ++ (show jcr) ++ ", " ++ (show ij)
      renderText renderer f p pText
      renderText renderer f (V2 0 1) jText

prepNextRender :: SDL.Renderer -> IO ()
prepNextRender renderer = do
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

runRender :: SDL.Renderer -> IO ()
runRender renderer = do
  liftIO $ SDL.present renderer
