{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Loop where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader)

import           Game.Types
  ( VideoConfig(..)
  , GameState(..)
  , EventQueue(..)
  , Scene(..)
  , QueueEvent(..) )
import           Game.World (Env)

import           Game.Effect.HasEventQueue (HasEventQueue(..))
-- import           Game.Effect.HasRunState (HasRunState, getRunState)
import           Game.Effect.HasScene (HasScene(..))
import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
import           Game.Effect.Renderer (Renderer, clearScreen, drawScreen)

import           Game.Wrapper.SDLInput (SDLInput, pollEvents)
import           Game.Wrapper.SDLTime (SDLTime, nextTick)
import           Game.Wrapper.Apecs (Apecs, runGC, runSystem)

import           Game.System.FixedTime (accumulateFixedTime, clearFixedTime, getFixedTime)
import           Game.System.Input (stepSDLInput, stepInputSystem)
import           Game.System.Audio (stepAudioQueue)
import           Game.System.Render (stepRender)

import           Game.Util.Constants (dT)

-- update physics multiple times if time step is less than frame update time
innerStep :: ( MonadReader Env m
             , SDLInput        m
             , SDLTime         m
             , Renderer        m
             , Apecs           m )
             -- , HasScene        m
             -- , HasEventQueue   m
             -- , HasVideoConfig  m
             -- , MonadIO         m )
          => Double -> [QueueEvent] -> m [QueueEvent]
innerStep acc events = do
  if (acc < dT)
  then return events
  -- when we've accumulated a fixed step update
  else do
    -- events' <- stepInputSystem events -- maintain key held or released updates
      -- >>= stepPlayerState  -- run updates based on input map
      -- >>= stepPhysicsSystem -- physics update
      -- >>= stepCollisionSystem
      -- >>= stepCameraPhysics
    -- -- update flow meter
    -- stepFlowMeter
    -- -- update player "action"
    -- stepPlayerAction
    -- clear away the fixed time we've accumulated
    clearFixedTime
    -- get next fixed time for update
    (_, acc') <- getFixedTime
    -- recurse if we need to run another fixed step update
    innerStep acc' events

mainLoop :: ( MonadReader Env m
            , SDLInput        m
            , SDLTime         m
            , Renderer        m
            , Apecs           m
            , HasScene        m
            , HasEventQueue   m
            , HasVideoConfig  m
            , MonadIO         m
            ) => m ()
mainLoop = do
  -- prep screen for next render
  clearScreen
  -- get next time tick from SDL
  nextTime   <- nextTick
  -- update player input button-key keystate-value map
  stepSDLInput
  -- accumulate fixed time for updates
  -- accumulateFixedTime nextTime
  -- get fixed time for inner step
  (_, acc) <- getFixedTime
  -- run inner step
  innerStep acc []
  -- effectEvents <- innerStep acc []
  -- setEvents effectEvents
  -- add all entities to render
  stepRender
  -- play audio
  stepAudioQueue
  -- run current render
  drawScreen
  -- clear out queue for next round
  -- setEvents []
  -- garbage collect. yes, every frame
  runGC
  -- loop if game still running
  scene <- getScene
  case scene of
    Scene'Quit  -> return ()
    _           -> mainLoop
