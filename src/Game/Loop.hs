{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Loop where

import qualified SDL
import           SDL.Time (ticks)
import qualified SDL.Font as TTF (initialize)
import qualified SDL.Mixer as Mixer (openAudio, defaultAudio)
import           Apecs (runSystem, runGC)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)

import           Game.World (System', World)
import           Game.FixedTime (accumulateFixedTime, clearFixedTime, getFixedTime)
import           Game.Player (stepPlayerState, stepPlayerAction)
import           Game.Camera (stepCamera)
import           Game.FlowMeter (stepFlowMeter)
import           Game.Input (handleSDLInput, maintainAllInputs)
import           Game.Physics (stepPhysics)
import           Game.Audio (stepAudioQueue)
import           Game.Render (prepNextRender, stepRender, runRender)
import           Game.Init (initSystems)
import           Game.Constants (dT, initialSize)

-- update physics multiple times if time step is less than frame update time
innerStep :: System' ()
innerStep = do
  (t, acc) <- getFixedTime

  -- when we've accumulated a fixed step update
  when (acc >= dT) $ do
    -- maintain key held or released updates
    maintainAllInputs

    -- run updates based on input map
    stepPlayerState

    -- physics update
    stepPhysics

    -- update flow meter
    stepFlowMeter

    -- update player "action"
    stepPlayerAction

    -- update camera
    stepCamera

    -- clear away the fixed time we've accumulated
    clearFixedTime

    -- recurse if we need to run another fixed step update
    innerStep

outerStep :: Double -> [SDL.Event] -> SDL.Window -> SDL.Renderer -> System' ()
outerStep nextTime events window renderer = do
  -- update velocity based on arrow key presses
  mapM handleSDLInput events

  -- accumulate fixed time for updates
  accumulateFixedTime nextTime

  -- run updates
  innerStep

  -- render
  stepRender renderer

  -- play audio
  stepAudioQueue

  -- garbage collect. yes, every frame
  runGC

mainLoop :: SDL.Window -> SDL.Renderer -> World -> IO ()
mainLoop window renderer world = do
  prepNextRender renderer

  -- get next time tick from SDL
  nextTime <- fromIntegral <$> ticks :: IO Double

  -- collect events from SDL
  events <- SDL.pollEvents

  -- run main system
  runSystem (outerStep nextTime events window renderer) world

  -- run current render
  runRender renderer

  -- loop
  mainLoop window renderer world
