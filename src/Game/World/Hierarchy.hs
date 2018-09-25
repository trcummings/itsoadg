{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Game.World.Hierarchy
  ( hNewEntity
  , HierarchyCons(..)
  ) where

import Apecs (Entity, Set, Get, newEntity, modify)

import Game.World.TH (ECS, World)
import Game.Types    (Hierarchy(..))

data HierarchyCons c =
  forall c.
  (Get World IO c, Set World IO c) =>
  HierarchyCons c [HierarchyCons c]

setChildren :: Maybe [Entity] -> Hierarchy -> Hierarchy
setChildren etys@(Just _) h = h { _children = etys }
setChildren Nothing       h = h { _children = Nothing }

-- hGet :: (Get World IO cx, Get World IO cy)
--      => (cx, Hierarchy) -> ECS (cx, [cy])
-- hGet (cx', hierarchy) = do
--   case (_children hierarchy) of
--     Nothing   -> return (cx', [])
--     Just etys -> do
--       fcps <- mapM get etys :: ECS [cy]
--       return (cx', fcps)

hNewEntity :: Maybe Entity -> HierarchyCons c -> ECS Entity
hNewEntity mEty (HierarchyCons c xc) = do
  -- create new parent entity with hierarchy component
  ety  <- newEntity (c, Hierarchy { _parent   = mEty
                                  , _children = Nothing })
  -- create children with entity pointing to parent
  etys <- mapM (hNewEntity (Just ety)) xc
  -- update parent entity with children
  modify ety (setChildren $ Just etys)
  -- return entity
  return ety
