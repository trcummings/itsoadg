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
import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (MonadReader, ask)

import           Game.Types
  ( Env(..)
  , VideoConfig(..)
  , GameState(..)
  , EventQueue(..)
  , RunState(..)
  , QueueEvent(..) )
import           Game.World (System', World, SystemFn)

import           Game.Effect.Event
  ( Event
  , prependAndGetEvents
  , setEvents
  , byInputEvent
  , byAudioSystemEvent )
import           Game.Effect.Renderer (Renderer, clearScreen, drawScreen)
import           Game.Effect.HasRunState (HasRunState, getRunState)

import           Game.Wrapper.SDLInput (SDLInput, pollEvents)
import           Game.Wrapper.SDLTime (SDLTime, nextTick)
import           Game.Wrapper.Apecs (Apecs, runGC, runSystem)

import           Game.System.FixedTime (accumulateFixedTime, clearFixedTime, getFixedTime)
import           Game.System.Player (stepPlayerState, stepPlayerAction)
import           Game.System.Camera (stepCameraPhysics)
import           Game.System.FlowMeter (stepFlowMeter)
import           Game.System.Input (handleSDLInput, stepInputSystem)
import           Game.System.Physics (stepPhysicsSystem)
import           Game.System.Collision (stepCollisionSystem)
import           Game.System.Audio (stepAudioQueue)
import           Game.System.Render (stepRender)
import           Game.System.Init (initSystems)

import           Game.Util.Constants (dT, initialSize)

-- update physics multiple times if time step is less than frame update time
innerStep :: Double -> SystemFn
innerStep acc events = do
  if (acc < dT)
  then return events
  -- when we've accumulated a fixed step update
  else do
    events' <- stepInputSystem events -- maintain key held or released updates
      >>= stepPlayerState  -- run updates based on input map
      >>= stepPhysicsSystem -- physics update
      >>= stepCollisionSystem
      >>= stepCameraPhysics
    -- update flow meter
    stepFlowMeter
    -- update player "action"
    stepPlayerAction
    -- clear away the fixed time we've accumulated
    clearFixedTime
    -- get next fixed time for update
    (_, acc') <- getFixedTime
    -- recurse if we need to run another fixed step update
    innerStep acc' events'

outerStep :: Double -> [QueueEvent] -> SDL.Renderer -> System' ()
outerStep nextTime events renderer = do
  -- update player input button-key keystate-value map
  mapM handleSDLInput (filter byInputEvent events)
  -- accumulate fixed time for updates
  accumulateFixedTime nextTime
  -- get fixed time for inner step
  (_, acc) <- getFixedTime
  -- run updates
  effectEvents <- innerStep acc []
  -- render
  stepRender renderer
  -- play audio
  stepAudioQueue (filter byAudioSystemEvent effectEvents)
  return ()

-- mainLoop ::
--   ( MonadReader Env m
--   , SDLInput     m
--   , SDLTime      m
--   , Renderer     m
--   , Apecs        m
--   , Event        m
--   , HasRunState m
--   ) => World -> m ()
-- mainLoop world = do
--   -- prep screen for next render
--   clearScreen
--   -- get next time tick from SDL
--   nextTime   <- nextTick
--   -- collect events from SDL
--   sdlEvents  <- pollEvents
--   -- collect previous rounds' events + sdlEvents
--   -- prepend input events to queue events to handle input changes 1 frame earlier
--   events     <- prependAndGetEvents sdlEvents
--   renderer   <- sdlRenderer <$> ask
--   runSystem (outerStep nextTime events renderer) world
--   -- run current render
--   drawScreen
--   -- clear out queue for next round
--   setEvents []
--   -- garbage collect. yes, every frame
--   runGC
--   -- loop if game still running
--   gameRunState <- getRunState
--   case gameRunState of
--     RunState'Quitting -> return ()
--     RunState'Running  -> mainLoop world
