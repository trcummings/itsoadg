{-# LANGUAGE FlexibleContexts #-}

module Game.Wrapper.Apecs where

import qualified Apecs as ECS
import           Control.Monad.IO.Class (MonadIO)
import           System.Mem             (performMajorGC)

class Monad m => Apecs m where
  runSystem :: ECS.System w a -> w -> m a
  runGC     :: m ()

runSystem' :: MonadIO m => ECS.System w a -> w -> m a
runSystem' f w = ECS.liftIO $ ECS.runSystem f w

runGC' :: MonadIO m => m ()
runGC' = ECS.liftIO $ performMajorGC
