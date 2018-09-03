{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasVideoConfig where

import           Control.Monad.Reader (MonadReader, ask)

-- import           Game.World (Env)
import           Game.Types (GameEnv(..), VideoConfig(..))

class Monad m => HasVideoConfig m where
  getVideoConfig :: m VideoConfig

getVideoConfig' :: (HasVideoConfig m, MonadReader GameEnv m) => m VideoConfig
getVideoConfig' = _videoConfig <$> ask
