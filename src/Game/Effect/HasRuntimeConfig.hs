{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasRuntimeConfig where

-- import           Control.Monad.Reader (MonadReader, ask)
--
-- import           Game.World (Env)
-- import           Game.Types (GameEnv(..), RuntimeConfig(..))
--
-- class Monad m => HasRuntimeConfig m where
--   getRuntimeConfig :: m RuntimeConfig
--
-- getRuntimeConfig' :: (HasRuntimeConfig m, MonadReader Env m) => m RuntimeConfig
-- getRuntimeConfig' = envRuntimeConfig <$> ask
