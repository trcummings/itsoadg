{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Wrapper.Apecs where

import qualified Apecs               as ECS
import qualified Apecs.Core          as Core
import qualified Data.Vector.Unboxed as U
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)

import           Game.World (World, Env)
import           Game.Types (QueueEvent)
import           Game.Effect.HasECSWorld (HasECSWorld(..))

class Monad m => Apecs m where
  runSystem  :: ECS.System World a -> m a
  runGC      :: m ()
  cmap       :: forall cx cy. (ECS.Has World cx, ECS.Has World cy)
             => (cx -> cy) -> m ()
  qmap       :: forall cx cy. (ECS.Has World cx, ECS.Has World cy)
             => (cx -> (cy, [QueueEvent])) -> m [QueueEvent]

runSystem' :: (Apecs m, HasECSWorld m, MonadIO m)
           => ECS.System World a -> m a
runSystem' f = do
  world <- getECSWorld
  liftIO $ runReaderT (ECS.unSystem f) world

runGC' :: (Apecs m) => m ()
runGC' = runSystem $ ECS.runGC

cmap' :: ( Apecs m
         , ECS.Has World cx
         , ECS.Has World cy )
      => (cx -> cy) -> m ()
cmap' f = runSystem $ ECS.cmap f

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

qmap' :: (Apecs m, ECS.Has World cx, ECS.Has World cy)
      => (cx -> (cy, [QueueEvent])) -> m [QueueEvent]
qmap' f = runSystem $ emap f
