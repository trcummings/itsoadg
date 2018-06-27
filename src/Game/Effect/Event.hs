{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Event where

import           Control.Monad.State (MonadState(..), get, put)

import           Game.Types (GameState(..), EventQueue(..), QueueEvent(..))

class Monad m => Event m where
  prependAndGetEvents :: [QueueEvent] -> m [QueueEvent]
  setEvents :: [QueueEvent] -> m ()

prependAndGetEvents' :: (Event m, MonadState GameState m)
                     =>   [QueueEvent]
                     -> m [QueueEvent]
prependAndGetEvents' inputs = do
  EventQueue queueEvents <- gsEventQueue <$> get
  return $ inputs ++ queueEvents

setEvents' :: (Event m, MonadState GameState m) => [QueueEvent] -> m ()
setEvents' evts = do
  gs <- get
  put $ gs { gsEventQueue = EventQueue evts }

byInputEvent :: QueueEvent -> Bool
byInputEvent (InputEvent _) = True
byInputEvent _              = False

byAudioSystemEvent :: QueueEvent -> Bool
byAudioSystemEvent (AudioSystemEvent _) = True
byAudioSystemEvent _                    = False
