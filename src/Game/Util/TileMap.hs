module Game.Util.TileMap where

import           Linear (V2(..))

import           Game.Types (TileMap(..), TileType(..))


basicTilemap :: TileMap
basicTilemap = TileMap [
    [S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, S]
  , [S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S, S]
               ]

toTileList :: TileMap -> [(TileType, V2 Int)]
toTileList (TileMap tMap) = concat $
        map (\(idx1, row) ->
               map (\(idx2, tiletype) ->
                      ( tiletype
                      , V2 idx2 idx1 )
                   ) $ zip [0..] row
            ) $ zip [0..] tMap

basicTilemap' :: [(TileType, V2 Int)]
basicTilemap' = toTileList basicTilemap

getTileTypeAt :: TileMap -> V2 Int -> TileType
getTileTypeAt (TileMap tMap) (V2 x y) = tMap !! y !! x
