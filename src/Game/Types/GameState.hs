module Game.Types.GameState where

import           Game.Types.EventQueue (EventQueue)
import           Game.Types.TileMap (TileMap)

data RunState =
    RunState'Running
  | RunState'Quitting

instance Show RunState where
  show (RunState'Quitting) = "Quitting"
  show (RunState'Running)  = "Running"

data GameState = GameState
  { gsRunState   :: RunState
  , gsEventQueue :: EventQueue
  , gsTileMap    :: TileMap }
