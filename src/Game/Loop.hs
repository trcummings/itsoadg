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
import Game.Effect.Clock
  ( Clock(..)
  , accumulateFixedTime
  , clearFixedTime
  , getFixedTime )
import Game.Effect.Input (Input(..), processInputs)

import Game.Wrapper.Apecs (Apecs, runGC, runSystem)

-- import Game.System.Audio (stepAudioQueue)

import Game.Scene.Title (stepTitle, initTitle, renderTitle, cleanUpTitle)
import Game.Scene.Play  (stepPlay, initPlay, renderPlay, cleanUpPlay)

import Game.Util.Constants (dT)

-- update physics multiple times if time step is less than frame update time
innerStep :: ( Input           m
             , Clock           m
             , Apecs           m
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
    step s = do
      -- liftIO $ putStrLn $ show scene
      case s of
        Scene'Title -> stepTitle
        Scene'Play  -> stepPlay
        _           -> return ()

mainLoop :: ( Clock           m
            , Input           m
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
  processInputs
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
    Scene'Title -> renderTitle
    Scene'Play  -> renderPlay
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
          Scene'Title -> initTitle
          Scene'Play  -> do
            case scene of Scene'Title -> cleanUpTitle
                          _           -> return ()
            initPlay
          Scene'Quit  ->
            case scene of Scene'Title -> cleanUpTitle
                          Scene'Play  -> cleanUpPlay
                          _           -> return ()
          _           -> return ()
        setScene nextScene
