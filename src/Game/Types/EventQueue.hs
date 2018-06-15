module Game.Types.EventQueue where

import qualified SDL
import           Apecs (Entity)

import           Game.Types.Audio (AudioEvent)

data To = To Entity deriving Show
data From = From Entity deriving Show

data Dir = L | R deriving Show
data Motion = Pressed | Released deriving Show
data MovementCommand =
    Command'Move (Maybe Dir)
  | Command'Jump Motion
  deriving Show

data QueueEvent =
    AudioSystemEvent AudioEvent
  | InputEvent SDL.Event
  | CommandSystemEvent (To, From, MovementCommand)
  deriving Show

data EventQueue = EventQueue [QueueEvent]
