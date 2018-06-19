module Game.Types.TileMap where

data TileType =
    E -- empty
  | S -- solid
  | I -- interesting (a ramp of some kind maybe)
  deriving (Eq, Show)

data TileMap = TileMap [[TileType]] deriving Show
