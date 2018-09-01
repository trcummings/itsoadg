module Game.Types.GameState where

data Scene =
    Scene'Init
  | Scene'Title
  | Scene'Play
  | Scene'Quit
  deriving (Eq, Show)

data PhysicsTime = PhysicsTime
  { time  :: Double
  , accum :: Double }
  deriving Show

-- global timer
newtype GlobalTime =
  GlobalTime Double
  deriving Show

data GameState = GameState
  { _Scene        :: Scene
  , _NextScene    :: Scene
  , _GlobalClock  :: GlobalTime
  , _PhysicsClock :: PhysicsTime }
