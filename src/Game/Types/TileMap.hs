module Game.Types.TileMap where

data TileType = E | S deriving (Eq, Show)

data TileMap = TileMap [[TileType]] deriving Show
