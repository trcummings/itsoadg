{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasTileMap where

import           Game.Types (GameState(..), TileMap(..))
import           Game.Effect.HasGameState (HasGameState(..))

class Monad m => HasTileMap m where
  getTileMap :: m TileMap

getTileMap' :: (HasGameState m, HasTileMap m) => m TileMap
getTileMap' = do
  tMap <- gsTileMap <$> getGameState
  return tMap
