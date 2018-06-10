{-# LANGUAGE TypeFamilies #-}

module Game.Types.EventQueue where

import qualified SDL

import           Game.Types.Physics (Collision)
import           Game.Types.Audio (AudioEvent)

data QueueEvent =
    AudioSystemEvent AudioEvent
  | PhysicsSystemEvent Collision
  | InputEvent SDL.Event
  deriving Show

data EventQueue = EventQueue [QueueEvent]
