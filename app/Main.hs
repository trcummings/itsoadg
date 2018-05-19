{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Monad

import Data.Maybe (catMaybes)
import Foreign.C.Types
import Linear
import Apecs

import Data.Text (singleton)

import SDL.Vect
import SDL.Time (ticks)
import qualified SDL.Font as TTF
import SDL (($=))
import qualified SDL

import Paths_itsoadg (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

initialSize = V2 screenWidth screenHeight

newtype Position = Position (V2 CInt) deriving Show
instance Component Position where
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 CInt) deriving Show
instance Component Velocity where
  type Storage Velocity = Map Velocity

data Player = Player deriving Show
instance Component Player where
  type Storage Player = Unique Player

data Camera = Camera deriving Show
instance Component Camera where
  type Storage Camera = Unique Camera

data Texture = Texture SDL.Texture (V2 CInt)
instance Component Texture where
  type Storage Texture = Map Texture

data Gravity = Gravity
instance Component Gravity where
  type Storage Gravity = Map Gravity

data Font = Font [(Char, Texture)]
instance Component Font where
  type Storage Font = Map Font

-- accumulator for physics frame time updates
data PhysicsTime = PhysicsTime
  { time  :: Double
  , accum :: Double }
  deriving Show

instance Monoid PhysicsTime where
  mempty = PhysicsTime 0 0

instance Component PhysicsTime where
  type Storage PhysicsTime = Global PhysicsTime

-- global timer
newtype GlobalTime = GlobalTime Double deriving Show

instance Monoid GlobalTime where
  mempty = GlobalTime 0

instance Component GlobalTime where
  type Storage GlobalTime = Global GlobalTime


makeWorld "World" [
    ''Position
  , ''Velocity
  , ''Player
  , ''Texture
  , ''GlobalTime
  , ''PhysicsTime
  , ''Gravity
  , ''Camera
  , ''Font
  ]

type System' a = System World a

playerSpeed = 1
playerPos = V2 320 240
spriteSize = V2 32 32

fps :: Double
fps = 60

dT :: Double
dT = 1000 / fps

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

initSystems :: SDL.Renderer -> System' ()
initSystems renderer = void $ do
  spriteSheetTexture <- liftIO $ loadTexture renderer "assets/red_square.bmp"
  smallFont <- liftIO $ TTF.load "assets/04B_19__.TTF" 24
  let characters =  ['a'..'z'] ++ ['A'..'Z']++ ['0'..'9'] ++ [' ', ':', ',']
  fontMap <- liftIO $ mapM (\c -> do
        texture <- toTexture renderer =<< TTF.blended
          smallFont
          (V4 255 255 255 255)
          (singleton c)
        return (c, texture)
      ) $ characters
  -- after we convert our font to textures we dont need the resource anymore
  TTF.free smallFont

  newEntity ( -- player
      Player
    , Position playerPos
    , Velocity $ V2 0 0
    , spriteSheetTexture )

  newEntity ( -- small font
      Position $ V2 0 0
    , Font fontMap )

bumpX dirF = cmap $ \(Player, Velocity (V2 x _)) ->
  Velocity (V2 (x `dirF` playerSpeed) 0)
bumpY dirF = cmap $ \(Player, Velocity (V2 _ y)) ->
  Velocity (V2 0 (y `dirF` playerSpeed))

-- x axis
handleArrowEvent SDL.KeycodeA SDL.Pressed  = bumpX (-)
handleArrowEvent SDL.KeycodeA SDL.Released = bumpX (+)
handleArrowEvent SDL.KeycodeD SDL.Pressed  = bumpX (+)
handleArrowEvent SDL.KeycodeD SDL.Released = bumpX (-)
-- y axis
handleArrowEvent SDL.KeycodeW SDL.Pressed  = bumpY (-)
handleArrowEvent SDL.KeycodeW SDL.Released = bumpY (+)
handleArrowEvent SDL.KeycodeS SDL.Pressed  = bumpY (+)
handleArrowEvent SDL.KeycodeS SDL.Released = bumpY (-)
handleArrowEvent _ _ = return ()

handleEvent :: SDL.Event -> System' ()
handleEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      handleArrowEvent keyCode motion
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent
    _ -> return ()

runPhysics :: Double -> System' ()
runPhysics dT = do
  -- update position based on time and velocity
  cmap $ \(Position p, Velocity v) ->
    Position $ p + (((round dT) :: CInt) *^ v)

  -- clamp player position to screen edges
  cmap $ \(Player, Position (V2 x y)) -> Position $ V2
    ((min (screenWidth  - 32) . max 0 $ x) :: CInt)
    ((min (screenHeight - 32) . max 0 $ y) :: CInt)

updatePhysicsAccum :: Double -> System' ()
updatePhysicsAccum nextTime = do
  GlobalTime currentTime <- get global
  -- update global time
  cmap $ \(GlobalTime _) -> GlobalTime nextTime

  -- update physics frame time accumulator
  cmap $ \(PhysicsTime t acc) -> PhysicsTime
    { time = t
    -- clamp frameTime at 25ms
    , accum = acc + (min 25 $ nextTime - currentTime) }

-- update physics multiple times if time step is less than frame update time
runPhysicsLoop :: System' ()
runPhysicsLoop = do
  PhysicsTime t acc <- get global
  when (acc >= dT) $ do
    runPhysics dT
    cmap $ \(PhysicsTime t' acc') -> PhysicsTime
      { time = (t' + dT)
      , accum = (acc' - dT) }
    runPhysicsLoop


step :: Double -> [SDL.Event] -> SDL.Window -> SDL.Renderer -> System' ()
step nextTime events window renderer = do
  -- update velocity based on arrow key presses
  mapM handleEvent events

  -- update physics
  updatePhysicsAccum nextTime
  runPhysicsLoop

  -- render "player"
  cmapM_ $ \(Player, Position p, Velocity v, Texture t s) -> do
    liftIO $ renderTexture
      renderer
      (Texture t s)
      (P p)
      (Just $ SDL.Rectangle (P (V2 0 0)) spriteSize)

  -- render small font
  cmapM_ $ \(Font f, Position p) -> do
    cmapM_ $ \(Player, Position pp, Velocity pv) -> do
      let pText = "Player: " ++ (show pp) ++ ", " ++ (show pv)
          textures = catMaybes (map (\c -> lookup c f) pText)
          spacingMap = [xy | xy <- [1..700], xy `mod` 14 == 0]
          textPosMap = zip textures spacingMap
      mapM_ (\(Texture t s, pMod) -> do
        liftIO $ renderTexture
          renderer
          (Texture t s)
          (P $ p + (V2 pMod 0))
          (Just $ SDL.Rectangle (P (V2 0 0)) s)
            ) (take 24 textPosMap)

  -- garbage collect. yes, every frame
  runGC

appLoop :: SDL.Window -> SDL.Renderer -> World -> IO ()
appLoop window renderer world = do
  -- prep next render
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

  -- get next time tick from SDL
  nextTime <- ticks

  -- collect events from SDL
  events <- SDL.pollEvents

  -- run main system
  runSystem (step (fromIntegral nextTime) events window renderer) world

  -- run current render
  liftIO $ SDL.present renderer

  -- loop
  appLoop window renderer world

main :: IO ()
main = do
  SDL.initializeAll -- initialize all SDL systems
  TTF.initialize   -- initialize SDL.Font

  -- create window and renderer
  window <- SDL.createWindow "ITSOADG" SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- initialize Apecs world & add entities
  world <- initWorld
  runSystem (initSystems renderer) world

  -- start loop
  appLoop window renderer world
