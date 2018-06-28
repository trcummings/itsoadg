{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasVideoConfig where

import           Control.Monad.Reader (MonadReader, ask)

import           Game.Types (Env(..), VideoConfig(..))

class Monad m => HasVideoConfig m where
  getVideoConfig :: m VideoConfig

getVideoConfig' :: (HasVideoConfig m, MonadReader Env m) => m VideoConfig
getVideoConfig' = envVideoConfig <$> ask
