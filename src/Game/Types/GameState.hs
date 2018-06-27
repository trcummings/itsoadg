module Game.Types.GameState where

import           Game.Types.EventQueue (EventQueue)
import           Game.Types.TileMap (TileMap)

data RunningState =
    RunningState'Running
  | RunningState'Quitting

instance Show RunningState where
  show (RunningState'Quitting) = "Quitting"
  show (RunningState'Running)  = "Running"

data GameState = GameState
  { runningState   :: RunningState
  , eventQueue     :: EventQueue
  , currentTileMap :: TileMap }
