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
-- import qualified Apecs (runSystem, runGC)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (MonadReader, ask)

import           Game.Types (SDLConfig(..))
import           Game.World (System', World)
import           Game.Effect.Renderer (Renderer, clearScreen, drawScreen)
import           Game.Wrapper.SDLInput (SDLInput, pollEvents)
import           Game.Wrapper.SDLTime (SDLTime, nextTick)
import           Game.Wrapper.Apecs (Apecs, runGC, runSystem)
import           Game.FixedTime (accumulateFixedTime, clearFixedTime, getFixedTime)
import           Game.Player (stepPlayerState, stepPlayerAction)
import           Game.Camera (stepCamera)
import           Game.FlowMeter (stepFlowMeter)
import           Game.Input (handleSDLInput, maintainAllInputs)
import           Game.Physics (stepPhysics)
import           Game.Audio (stepAudioQueue)
import           Game.Render (stepRender)
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

outerStep :: Double -> [SDL.Event] -> SDL.Renderer -> System' ()
outerStep nextTime events renderer = do
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

mainLoop ::
  ( MonadReader SDLConfig m
  , SDLInput m
  , SDLTime  m
  , Renderer m
  , Apecs    m
  ) => World -> m ()
mainLoop world = do
  clearScreen

  -- get next time tick from SDL
  nextTime <- nextTick

  -- collect events from SDL
  events <- pollEvents

  -- run main system
  renderer <- sdlRenderer <$> ask
  runSystem (outerStep nextTime events renderer) world

  -- run current render
  drawScreen

  -- garbage collect. yes, every frame
  runGC

  -- loop
  mainLoop world
