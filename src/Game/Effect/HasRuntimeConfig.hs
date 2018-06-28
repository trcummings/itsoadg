{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasRuntimeConfig where

import           Control.Monad.Reader (MonadReader, ask)

import           Game.Types (Env(..), RuntimeConfig(..))

class Monad m => HasVideoConfig m where
  getRuntimeConfig :: m RuntimeConfig

getRuntimeConfig' :: (HasRuntimeConfig m, MonadReader Env m) => m RuntimeConfig
getRuntimeConfig' = envRuntimeConfig <$> ask

