{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Event where

import           Control.Monad.State (MonadState(..), get, put)

import           Game.Types (EventQueue(..), QueueEvent(..))

class Monad m => Event m where
  prependAndGetEvents :: [QueueEvent] -> m [QueueEvent]
  setEvents :: [QueueEvent] -> m ()

prependAndGetEvents' :: (Event m, MonadState EventQueue m) => [QueueEvent] -> m [QueueEvent]
prependAndGetEvents' inputs = do
  EventQueue queueEvents <- get
  return $ inputs ++ queueEvents

setEvents' :: (Event m, MonadState EventQueue m) => [QueueEvent] -> m ()
setEvents' evts = do
  put $ EventQueue evts

byInputEvent :: QueueEvent -> Bool
byInputEvent (InputEvent _) = True
byInputEvent _              = False

byAudioSystemEvent :: QueueEvent -> Bool
byAudioSystemEvent (AudioSystemEvent _) = True
byAudioSystemEvent _                    = False
