module Game.Types.GameState where

-- import           Game.Types.EventQueue (EventQueue)
-- import           Game.Types.TileMap (TileMap)

data Scene =
    Scene'Init
  | Scene'Title
  | Scene'Play
  | Scene'Quit
  deriving (Eq, Show)

data GameState = GameState
  { gsScene      :: Scene
  , gsNextScene  :: Scene }
  -- , gsEventQueue :: EventQueue
  -- , gsTileMap    :: TileMap }
