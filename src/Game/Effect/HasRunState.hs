{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasRunState where

import           Game.Types (GameState(..), RunState(..))
import           Game.Effect.HasGameState (HasGameState)

class Monad m => HasRunState m where
  getRunState :: m RunState

getRunState' :: (HasGameState m, HasRunState m) => m RunState
getRunState' = undefined
