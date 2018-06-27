module Game.Types.GameState where

import           Game.Types.EventQueue (EventQueue)


data RunningState =
    RunningState'Running
  | RunningState'Quitting

instance Show RunningState where
  show (RunningState'Quitting) = "Quitting"
  show (RunningState'Running)  = "Running"

data GameState = GameState
  { runningState :: RunningState
  , eventQueue   :: EventQueue }
