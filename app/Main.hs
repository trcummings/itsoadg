{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Monad 
import Foreign.C.Types
import Linear
import Apecs
import SDL

import Paths_itsoadg (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

initialSize = V2 screenWidth screenHeight

newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

makeWorld "World" [''Position, ''Velocity, ''Player]

type System' a = System World a

playerSpeed = 170
playerPos = V2 0 (-120)
xmin = -100
xmax = 100

initSystems :: System' ()
initSystems = void $ newEntity (Player, Position playerPos, Velocity $ V2 0 0)

bumpX dirF = cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x `dirF` playerSpeed) 0)
bumpY dirF = cmap $ \(Player, Velocity (V2 _ y)) -> Velocity (V2 0 (y `dirF` playerSpeed))

-- x axis
handleArrowEvent KeycodeA Pressed  = bumpX (-)
handleArrowEvent KeycodeA Released = bumpX (+)
handleArrowEvent KeycodeD Pressed  = bumpX (+)
handleArrowEvent KeycodeD Released = bumpX (-)
-- y axis
handleArrowEvent KeycodeW Pressed  = bumpY (+)
handleArrowEvent KeycodeW Released = bumpY (-)
handleArrowEvent KeycodeS Pressed  = bumpY (-)
handleArrowEvent KeycodeS Released = bumpY (+)
handleArrowEvent _ _ = return ()

handleEvent :: Event -> System' ()
handleEvent event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      handleArrowEvent keyCode motion
      where
        keyCode = keysymKeycode $ keyboardEventKeysym keyboardEvent
        motion  = keyboardEventKeyMotion keyboardEvent
    _ -> return ()

handleEvents :: Renderer -> System' ()
handleEvents renderer = do
  events <- pollEvents
  mapM handleEvent events
  cmap $ \(Position p, Velocity v) -> Position (p + v)
  cmapM_ $ \(Player, Position p, Velocity v) -> do
    liftIO $ rendererDrawColor renderer $= V4 0 0 0 0
    liftIO $ clear renderer
    liftIO $ present renderer
    liftIO $ putStrLn $ show (p, v)
  runGC

appLoop :: Renderer -> World -> IO ()
appLoop renderer world = do
  -- handle events
  runSystem (handleEvents renderer) world
  -- loop
  appLoop renderer world

main :: IO ()
main = do
  initializeAll
  window <- createWindow "In the Shadow of a Dead God" defaultWindow { windowInitialSize = initialSize }
  renderer <- createRenderer window (-1) defaultRenderer
  
  world <- initWorld
  runSystem initSystems world
  appLoop renderer world
