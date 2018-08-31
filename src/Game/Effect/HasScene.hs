{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasScene where

import           Game.Types (GameState(..), Scene(..))
import           Game.Effect.HasGameState (HasGameState(..))

class Monad m => HasScene m where
  getScene     :: m Scene
  getNextScene :: m Scene

getScene' :: (HasGameState m, HasScene m) => m Scene
getScene' = _Scene <$> getGameState

getNextScene' :: (HasGameState m, HasScene m) => m Scene
getNextScene' = _NextScene <$> getGameState
