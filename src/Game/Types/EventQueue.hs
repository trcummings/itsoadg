module Game.Types.EventQueue where

import qualified SDL
import           Apecs (Entity)
import           Linear (V2(..))

import           Game.Types.Audio (AudioEvent)
import           Game.Types.Util (Unit(..))

data To = To Entity deriving Show
data From = From Entity deriving Show

data Dir = L | R deriving Show
data Motion = Pressed | Released deriving Show
data MovementCommand =
    Command'Move (Maybe Dir)
  | Command'Jump Motion
  deriving Show

data RenderEvent =
  Line (V2 Unit) (V2 Unit)
  deriving Show

data QueueEvent =
    AudioSystemEvent AudioEvent
  | InputEvent SDL.Event
  | CommandSystemEvent (To, From, MovementCommand)
  | RenderSystemEvent RenderEvent
  deriving Show

data EventQueue = EventQueue [QueueEvent]
