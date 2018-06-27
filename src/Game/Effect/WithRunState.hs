{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.WithRunState where

import           Control.Monad.State (MonadState(..), get)

import           Game.Types (GameState(..), RunState(..))

class Monad m => WithRunState m where
  getRunState :: m RunState

getRunState' :: (WithRunState m, MonadState GameState m) => m RunState
getRunState' = gsRunState <$> get
