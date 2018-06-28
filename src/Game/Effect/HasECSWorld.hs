{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasECSWorld where

import           Control.Monad.Reader (MonadReader, ask)

import           Game.World (Env, World)
import           Game.Types (GameEnv(..))

class Monad m => HasECSWorld m where
  getECSWorld :: m World

getECSWorld' :: (HasECSWorld m, MonadReader Env m) => m World
getECSWorld' = envECSWorld <$> ask
