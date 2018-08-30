module Game.Types.GameState where

data Scene =
    Scene'Init
  | Scene'Title
  | Scene'Play
  | Scene'Quit
  deriving (Eq, Show)

data GameState = GameState
  { gsScene      :: Scene
  , gsNextScene  :: Scene }
