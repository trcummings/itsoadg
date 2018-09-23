{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Game.World.Hierarchy
  ( newHierarchicalEntity
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

newHierarchicalEntity :: Maybe Entity -> HierarchyCons c -> ECS Entity
newHierarchicalEntity mEty (HierarchyCons c xc) = do
  -- create new parent entity with hierarchy component
  ety  <- newEntity (c, Hierarchy { _parent   = mEty
                                  , _children = Nothing })
  -- create children with entity pointing to parent
  etys <- mapM (newHierarchicalEntity (Just ety)) xc
  -- update parent entity with children
  modify ety (setChildren $ Just etys)
  -- return entity
  return ety
