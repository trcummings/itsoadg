{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Loop where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)

import Game.Types
  ( VideoConfig(..)
  , GameState(..)
  , EventQueue(..)
  , Scene(..)
  , QueueEvent(..) )
import Game.World (Env)

import Game.Effect.SceneManager (SceneManager(..))
import Game.Effect.HasVideoConfig (HasVideoConfig(..))
import Game.Effect.Renderer (Renderer(..), clearScreen, drawScreen)
import Game.Effect.Clock (Clock(..), accumulateFixedTime, clearFixedTime, getFixedTime)

import Game.Wrapper.SDLInput (SDLInput, pollEvents)
import Game.Wrapper.Apecs (Apecs, runGC, runSystem)

import Game.System.Input (stepSDLInput)
-- import Game.System.Audio (stepAudioQueue)

import Game.Scene.Title (titleStep, titleTransition, titleRender)

import Game.Util.Constants (dT)

-- update physics multiple times if time step is less than frame update time
innerStep :: ( SDLInput        m
             , Apecs           m
             , Clock           m
             , HasVideoConfig  m
             , SceneManager    m
             , MonadIO         m )
          => Double -> [QueueEvent] -> Scene -> m [QueueEvent]
innerStep acc events scene = do
  if (acc < dT)
  then return events
  -- when we've accumulated a fixed step update
  else do
    step scene
    -- clear away the fixed time we've accumulated
    clearFixedTime
    -- get next fixed time for update
    (_, acc') <- getFixedTime
    -- recurse if we need to run another fixed step update
    innerStep acc' events scene
  where
    step scene = do
      -- liftIO $ putStrLn $ show scene
      case scene of
        Scene'Title -> titleStep
        _           -> return ()

mainLoop :: ( SDLInput        m
            , Clock           m
            , Renderer        m
            , Apecs           m
            , SceneManager    m
            , HasVideoConfig  m
            , MonadIO         m
            ) => m ()
mainLoop = do
  -- prep screen for next render
  clearScreen
  -- update player input button-key keystate-value map
  stepSDLInput
  -- accumulate fixed time for updates
  accumulateFixedTime
  -- get fixed time for inner step
  (_, acc) <- getFixedTime
  -- run inner step
  scene <- getScene
  innerStep acc [] scene
  -- effectEvents <- innerStep acc []
  -- setEvents effectEvents
  case scene of
    Scene'Title -> titleRender
    _           -> return ()
  -- play audio
  -- stepAudioQueue
  -- run current render, swap background buffer
  drawScreen
  -- garbage collect. yes, every frame
  runGC
  -- loop if game still running
  nextScene <- getNextScene
  stepScene scene nextScene
  case scene of
    Scene'Quit  -> return ()
    _           -> mainLoop
  where
    stepScene scene nextScene = do
      when (nextScene /= scene) $ do
        case nextScene of
          Scene'Title -> titleTransition
          _           -> return ()
        setScene nextScene
