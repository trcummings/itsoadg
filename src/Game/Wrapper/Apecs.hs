{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Wrapper.Apecs where

import qualified Apecs               as ECS
import qualified Apecs.Core          as Core
import qualified Data.Vector.Unboxed as U
import           Control.Monad.IO.Class (MonadIO)
import           System.Mem             (performMajorGC)

import           Game.World (World)
import           Game.Types (QueueEvent)

class Monad m => Apecs m where
  runSystem :: ECS.System w a -> w -> m a
  runGC     :: m ()

runSystem' :: MonadIO m => ECS.System w a -> w -> m a
runSystem' f w = ECS.liftIO $ ECS.runSystem f w

runGC' :: MonadIO m => m ()
runGC' = ECS.liftIO $ performMajorGC

emap :: forall cx cy. (ECS.Has World cx, ECS.Has World cy)
     => (cx -> (cy, [QueueEvent]))
     -> ECS.System World [QueueEvent]
emap f = do
  sx :: ECS.Storage cx <- ECS.getStore
  sy :: ECS.Storage cy <- ECS.getStore
  sl <- ECS.liftIO $ Core.explMembers sx
  qs <- ECS.liftIO $ U.foldM (\qs e -> do
                                 x <- Core.explGet sx e
                                 let (x', qs') = f x
                                 Core.explSet sy e x'
                                 return $ qs ++ qs' ) [] sl
  return qs
