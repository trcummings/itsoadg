{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Monad

import Foreign.C.Types
import Linear
import Apecs

import SDL.Vect
import SDL.Time (ticks)
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

-- global timer
data Time = Time
  { currentTime :: Double   --miliseconds
  , stepTime    :: Double } --miliseconds
  deriving Show

-- accumulator for physics frame time updates
newtype PhysAccum =
  PhysAccum Double
  deriving Show

instance Monoid PhysAccum where
  mempty = PhysAccum 0

instance Component PhysAccum where
  type Storage PhysAccum = Global PhysAccum

instance Monoid Time where
  mempty = Time 0 0.1

instance Component Time where
  type Storage Time = Global Time


makeWorld "World" [
    ''Position
  , ''Velocity
  , ''Player
  , ''Texture
  , ''Time
  , ''PhysAccum
  , ''Gravity
  , ''Camera
  ]

type System' a = System World a

playerSpeed = 10
playerPos = V2 320 240
spriteSize = V2 32 32

fps :: Double
fps = 60

dT :: Double
dT = 1000 / fps


loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))


initSystems :: Texture -> System' ()
initSystems t = void $
  newEntity (
     Player
   , Position playerPos
   , Velocity $ V2 0 0
   , t)

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

updatePhysicsAccum :: System' ()
updatePhysicsAccum = do
  Time _ sT <- get global
  -- cmapM_ $ \(PhysAccum acc) -> do
    -- liftIO $ putStr "accumulator: "
    -- liftIO $ putStrLn $ show acc
  cmap $ \(PhysAccum acc) -> PhysAccum (sT + acc)

-- update physics multiple times if time step is less than frame update time
runPhysicsLoop :: System' ()
runPhysicsLoop = do
  Time _ fT     <- get global
  PhysAccum acc <- get global
  if (acc < dT)
  then return ()
  else do
    -- liftIO $ putStrLn "doing physics!"
    runPhysics fT
    cmap $ \(PhysAccum acc2) -> PhysAccum (acc2 - dT)
    runPhysicsLoop

step :: Double -> [SDL.Event] -> SDL.Renderer -> System' ()
step nextTime events renderer = do
  -- update velocity based on arrow key presses
  mapM handleEvent events

  -- update global timer
  cmap $ \(Time cT sT) ->
    Time { currentTime = nextTime, stepTime = min 25 $ nextTime - cT }

  -- cmapM_ $ \(Time cT fT) -> do
  --   liftIO $ putStrLn "================="
  --   liftIO $ putStr "before physics: "
  --   liftIO $ putStrLn $ show fT

  -- update physics
  updatePhysicsAccum
  runPhysicsLoop

  -- cmapM_ $ \(Time cT fT) -> do
  --   liftIO $ putStr "after physics: "
  --   liftIO $ putStrLn $ show fT
  --   liftIO $ putStrLn "================="

  -- render "player"
  cmapM_ $ \(Player, Position p, Velocity v, Texture t s) -> do
    liftIO $ renderTexture
      renderer
      (Texture t s)
      (P p)
      (Just $ SDL.Rectangle (P (V2 0 0)) spriteSize)

  -- garbage collect. yes, every frame
  runGC

appLoop :: SDL.Renderer -> World -> IO ()
appLoop renderer world = do
  -- prep next render
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

  -- get next time tick from SDL
  nextTime <- ticks

  -- collect events from SDL
  events <- SDL.pollEvents

  -- run main system
  runSystem (step (fromIntegral nextTime) events renderer) world

  -- run current render
  liftIO $ SDL.present renderer

  -- loop
  appLoop renderer world

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "In the Shadow of a Dead God" SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  spriteSheetTexture <- loadTexture renderer "assets/red_square.bmp"

  world <- initWorld
  runSystem (initSystems spriteSheetTexture) world
  appLoop renderer world
