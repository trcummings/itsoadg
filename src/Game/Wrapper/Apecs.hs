{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Wrapper.Apecs where

import qualified Apecs               as ECS
import qualified Apecs.Core          as Core
import qualified Data.Vector.Unboxed as U
import           Control.Monad (mapM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)

import           Game.World (World, Env)
import           Game.Types (QueueEvent)
import           Game.Effect.HasECSWorld (HasECSWorld(..))

class Monad m => Apecs m where
  runSystem  :: ECS.System World a -> m a

  runGC      :: m ()

  newEntity  :: forall c. ( Core.Store (ECS.Storage c)
                          , ECS.Has World c
                          , ECS.Has World ECS.EntityCounter )
             => c -> m ECS.Entity

  cmap       :: forall cx cy. (ECS.Has World cx, ECS.Has World cy)
             => (cx -> cy) -> m ()

  cmapM      :: forall c a. ECS.Has World c => (c -> m a) -> m [a]

  cmapM_     :: forall c a. ECS.Has World c => (c -> m a) -> m ()

  qmap       :: forall cx cy. (ECS.Has World cx, ECS.Has World cy)
             => (cx -> (cy, [QueueEvent])) -> m [QueueEvent]

  get        :: forall c. ECS.Has World c
             => Core.Entity -> m c

  set        :: forall c. ECS.Has World c
             => Core.Entity -> c -> m ()

  getAll     :: forall c. ECS.Has World c => m [c]

  destroy    :: forall c. ECS.Has World c
             => Core.Entity -> c -> m ()

  modify     :: forall c. ECS.Has World c
             => Core.Entity -> (c -> c) -> m ()

  exists     :: forall c. ECS.Has World c
             => Core.Entity -> c -> m Bool

runSystem' :: (Apecs m, HasECSWorld m, MonadIO m)
           => ECS.System World a -> m a
runSystem' f = do
  world <- getECSWorld
  liftIO $ runReaderT (ECS.unSystem f) world

runGC' :: Apecs m => m ()
runGC' = runSystem $ ECS.runGC

newEntity' :: (Apecs m, ECS.Has World c) => c -> m ECS.Entity
newEntity' c = runSystem $ ECS.newEntity c

cmap' :: (Apecs m, ECS.Has World cx, ECS.Has World cy)
      => (cx -> cy) -> m ()
cmap' f = runSystem $ ECS.cmap f

cmapM' :: (Apecs m, ECS.Has World c) => (c -> m a) -> m [a]
cmapM' f = do
   all <- getAll
   mapM f all

cmapM_' :: (Apecs m, ECS.Has World c) => (c -> m a) -> m ()
cmapM_' f = do
  all <- getAll
  mapM_ f all
  return ()

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

get' :: (Apecs m, ECS.Has World c)
     => Core.Entity -> m c
get' e = runSystem $ ECS.get e

set' :: (Apecs m, ECS.Has World c)
     => Core.Entity -> c -> m ()
set' e c = runSystem $ ECS.set e c

getAll' :: (Apecs m, ECS.Has World c) => m [c]
getAll' = do
  all <- runSystem $ ECS.getAll
  return all

destroy' :: (Apecs m, ECS.Has World c) => Core.Entity -> c -> m ()
destroy' e c = runSystem $ ECS.destroy e c

modify' :: (Apecs m, ECS.Has World c) => Core.Entity -> (c -> c) -> m ()
modify' e f = runSystem $ ECS.modify e f

exists' :: (Apecs m, ECS.Has World c) => Core.Entity -> c -> m Bool
exists' e c = runSystem $ ECS.exists e c
