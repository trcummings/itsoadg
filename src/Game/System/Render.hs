{-# LANGUAGE ScopedTypeVariables #-}

module Game.System.Render where

import qualified SDL
import           SDL (($=), Point(..))
import qualified Animate
import qualified SDL.Image as Image (load)
import qualified SDL.Raw.Video as Raw (allocFormat, freeFormat, convertSurface)
import qualified SDL.Internal.Numbered as Numbered
import           Foreign.C.Types (CInt)
import           GHC.Int (Int32)
import           Linear (V4(..), V2(..), (^/))
-- import           Apecs (cmapM_, cmap, set)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (catMaybes)
import           Data.Coerce (coerce)
import           Paths_itsoadg (getDataFileName)

import           Game.World (System')
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

renderFlowMeter :: SDL.Renderer -> Position -> FlowMeter -> IO ()
renderFlowMeter renderer (Position p) flow = do
  let boxHeight     = 5
      baseHeight    = boxHeight - (boxHeight * Unit (baseFlow flow / flowLimit flow))
      currentHeight = (boxHeight * Unit (currentFlow flow / flowLimit flow))
  -- render meter
  liftIO $ SDL.fillRect
    renderer
    ( makeRect
      (toPixels <$> V2 0.5 (boxHeight - currentHeight + 1))
      (toPixels <$> V2 1 currentHeight) )
  -- render baseFlow line
  liftIO $ SDL.drawLine
    renderer
    (P (toPixels <$> V2 0.5 (baseHeight + 1)))
    (P (toPixels <$> V2 1.5 (baseHeight + 1)))
  -- render meter outline
  liftIO $ SDL.drawRect
    renderer
    ( makeRect
      (toPixels <$> V2 0.5 1)
      (toPixels <$> V2 1 boxHeight) )

renderFromCamera :: (Apecs m, HasVideoConfig m, MonadIO m)
                 => (Camera, Position) -> m ()
renderFromCamera (Camera _ _, Position cameraPos) = do
  renderer   <- vcRenderer <$> getVideoConfig
  -- render "player"
  cmapM_ $ \(Player spa, Position p, Velocity v, SpriteSheet ss ap) -> do
    let animations = Animate.ssAnimations ss :: Animations PlayerKey
        location   = Animate.currentLocation animations ap
    liftIO $ renderSprite renderer ss location (p - cameraPos)

  -- render bounding boxes
  cmapM_ $ \(BoundingBox bb, Position p) -> do
    liftIO $ SDL.rendererDrawColor renderer $= V4 255 0 maxBound maxBound
    liftIO $ SDL.drawRect
      renderer
      (makeRect (toPixels <$> (p - cameraPos)) (toPixels <$> bb))

  -- render broad phase bounding box
  cmapM_ $ \(bb@(BoundingBox _), p@(Position _), v@(Velocity _)) -> do
    let aabb = broadPhaseAABB bb p v
    liftIO $ SDL.rendererDrawColor renderer $= V4 maxBound maxBound 0 255
    liftIO $ SDL.drawRect
      renderer
      (makeRect (toPixels <$> ((center aabb) - cameraPos)) (toPixels <$> (dims aabb)))

  liftIO $ mapM_ (\(ttype, pos) -> do
                     let pos' = (Unit <$> fromIntegral <$> pos) :: V2 Unit
                     case ttype of
                       E -> return ()
                       S -> do
                         liftIO $
                           SDL.rendererDrawColor renderer $= V4 255 0 maxBound maxBound
                         liftIO $
                           SDL.drawRect
                           renderer
                           (makeRect (toPixels <$> (pos' - cameraPos)) (V2 32 32))
                 ) basicTilemap'

renderAllFlowMeter :: (Apecs m, HasVideoConfig m, MonadIO m)
                   => (Font, Position)
                   -> (Player, FlowMeter)
                   -> m ()
renderAllFlowMeter (Font f, Position p)
                   (Player _, flow@(FlowMeter _ _ _ _)) = do
   renderer   <- vcRenderer <$> getVideoConfig
   -- render meter text
   liftIO $ renderText renderer f p "Flow"
   -- render meter
   liftIO $ renderFlowMeter renderer (Position p) flow

renderFlow :: (Apecs m, HasVideoConfig m, MonadIO m) => (Font, Position) -> m ()
renderFlow fp = cmapM_ $ renderAllFlowMeter fp

renderReticule :: (Apecs m, HasVideoConfig m, MonadIO m)
               => MousePosition -> m ()
renderReticule (MousePosition (V2 x y)) = do
    renderer   <- vcRenderer <$> getVideoConfig
    -- get positions relative to middle of screen
    let w = coerce (32 * screenWidth  / 2) :: Double
        h = coerce (32 * screenHeight / 2) :: Double
        -- coerce mouse position
        x_ = fromIntegral x :: Double
        y_ = fromIntegral y :: Double
        -- get polar coordinates
        r = sqrt $ (((x_ - w) ^ 2) + ((y_ - h) ^ 2))
        theta = 2 * atan ((y_ - h) / (x_ - w + r))
        -- clamp polar radius
        r' = 4 * 32
        -- convert back to cartesian coordinates
        x' = r' * cos theta
        y' = r' * sin theta
        -- shift new coordinates back to screen position
        px = round ((x' + w) :: Double) :: CInt
        py = round ((y' + h) :: Double) :: CInt
        -- lil bounding box
        bb = toPixels <$> (V2 (Unit 0.25) (Unit 0.25))
    liftIO $ SDL.rendererDrawColor renderer $= V4 161 62 180 maxBound
    liftIO $ SDL.drawRect renderer (Just $ SDL.Rectangle (P $ V2 px py) bb)

stepRender :: (Apecs m, HasVideoConfig m, MonadIO m) => m ()
stepRender = do
  cmapM_ renderFromCamera
  cmapM_ renderFlow
  cmapM_ renderReticule


prepNextRender :: SDL.Renderer -> IO ()
prepNextRender renderer = do
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

runRender :: SDL.Renderer -> IO ()
runRender renderer = do
  liftIO $ SDL.present renderer
