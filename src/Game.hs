{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game (main) where

import qualified SDL
import           Apecs (runSystem, runGC)
import           SDL.Time (ticks)
import qualified SDL.Font as TTF (initialize)
import           Control.Monad.IO.Class (liftIO)

import           Game.World (System', World, initWorld)
import           Game.Physics (handleEvent, updatePhysicsAccum, runPhysicsLoop)
import           Game.Render (prepNextRender, stepRender, runRender)
import           Game.Init (initSystems)
import           Game.Constants (initialSize)

step :: Double -> [SDL.Event] -> SDL.Window -> SDL.Renderer -> System' ()
step nextTime events window renderer = do
  -- update velocity based on arrow key presses
  mapM handleEvent events

  -- update physics
  updatePhysicsAccum nextTime
  runPhysicsLoop

  -- render
  stepRender renderer

  -- garbage collect. yes, every frame
  runGC

appLoop :: SDL.Window -> SDL.Renderer -> World -> IO ()
appLoop window renderer world = do
  prepNextRender renderer

  -- get next time tick from SDL
  nextTime <- ticks

  -- collect events from SDL
  events <- SDL.pollEvents

  -- run main system
  runSystem (step (fromIntegral nextTime) events window renderer) world

  -- run current render
  runRender renderer

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
