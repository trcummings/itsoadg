{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasEventQueue where

import           Game.Types
  ( Env(..)
  , GameState(..)
  , EventQueue(..)
  , QueueEvent(..) )
import           Game.Effect.HasGameState (HasGameState(..))

class Monad m => HasEventQueue m where
  getEvents           :: m [QueueEvent]
  prependAndGetEvents ::   [QueueEvent] -> m [QueueEvent]
  setEvents           ::   [QueueEvent] -> m ()

getEvents' :: (HasGameState m, HasEventQueue m)
           => m [QueueEvent]
getEvents' = do
  EventQueue events <- gsEventQueue <$> getGameState
  return events

prependAndGetEvents' :: (HasGameState m, HasEventQueue m)
                     =>   [QueueEvent]
                     -> m [QueueEvent]
prependAndGetEvents' events = do
  events' <- getEvents'
  return $ events ++ events'

setEvents' :: (HasGameState m, HasEventQueue m)
           => [QueueEvent]
           -> m ()
setEvents' evts = do
  setGameState (\gs -> gs { gsEventQueue = EventQueue evts })
  return ()

byInputEvent :: QueueEvent -> Bool
byInputEvent (InputEvent _) = True
byInputEvent _              = False

byAudioSystemEvent :: QueueEvent -> Bool
byAudioSystemEvent (AudioSystemEvent _) = True
byAudioSystemEvent _                    = False
