module Game.Types.TileMap where

import           Apecs (Entity)

data TileType =
    E -- empty
  | S -- solid
  | I -- interesting (a ramp of some kind maybe)
  deriving (Eq, Show)

data Tile = Tile
  { tileType :: TileType
  , entities :: [Entity] }
  deriving Show

data TileMap = TileMap [[TileType]]

data TileMap' = TileMap' { tiles :: [[Tile]] } deriving Show
