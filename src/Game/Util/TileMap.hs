module Game.Util.TileMap where

import           Linear (V2(..), (*^))
import           Control.Lens ((^?), element)
import           Data.Coerce (coerce)
import           Data.Maybe (catMaybes)
import           Data.List (sortOn)

import           Game.Util.Constants (frameDeltaSeconds)
import           Game.Util.AABB (aabbMin, aabbMax)
import           Game.Types
  ( TileMap(..)
  , TileType(..)
  , AABB(..)
  , RaycastHit(..)
  , Unit(..)
  , Velocity(..)
  , Position(..) )


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
  , [S, E, E, E, E, E, E, E, E, E, E, E, E, S, S, S, S, E, E, S]
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

getTileTypeAt :: TileMap -> V2 Int -> Maybe TileType
getTileTypeAt (TileMap tMap) (V2 x y) = tMap ^? element y . element x

getIntersectingTiles :: TileMap -> AABB -> [(TileType, V2 Unit)]
getIntersectingTiles tMap box =
  let (V2 xMin yMin) =
        fromIntegral <$> (floor   <$> (coerce (aabbMin box) :: V2 Double)) :: V2 Int
      (V2 xMax yMax) =
        fromIntegral <$> (ceiling <$> (coerce (aabbMax box) :: V2 Double)) :: V2 Int
     -- filter out empty tiles
  in filter (\(t, _) -> not $ E == t) $
     catMaybes [ (Just $ \t -> (t, (Unit <$> (fromIntegral <$> V2 x y) :: V2 Unit)))
                 <*> (getTileTypeAt tMap $ V2 x y)
               | x <- [xMin..xMax]
               , y <- [yMin..yMax] ]
