{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.World
  ( World
  , System'
  , initWorld ) where

import Apecs

import Game.Types
  ( Position
  , Velocity
  , Acceleration
  , BoundingBox
  , Player
  , Camera
  , Floor
  , Texture
  , Gravity
  , Friction
  , Font
  , Collisions
  , PhysicsTime(..)
  , GlobalTime(..)
  , Jump(..) )

instance Component Position where
  type Storage Position = Map Position

instance Component Velocity where
  type Storage Velocity = Map Velocity

instance Component Acceleration where
  type Storage Acceleration = Map Acceleration

instance Component BoundingBox where
  type Storage BoundingBox = Map BoundingBox

instance Component Player where
  type Storage Player = Unique Player

instance Component Camera where
  type Storage Camera = Unique Camera

instance Component Floor where
  type Storage Floor = Map Floor

instance Component Texture where
  type Storage Texture = Map Texture

instance Component Gravity where
  type Storage Gravity = Map Gravity

instance Component Friction where
  type Storage Friction = Map Friction

instance Component Font where
  type Storage Font = Map Font

instance Component Collisions where
  type Storage Collisions = Map Collisions

instance Monoid PhysicsTime where
  mempty = PhysicsTime 0 0
instance Component PhysicsTime where
  type Storage PhysicsTime = Global PhysicsTime

instance Monoid GlobalTime where
  mempty = GlobalTime 0
instance Component GlobalTime where
  type Storage GlobalTime = Global GlobalTime

instance Component Jump where
  type Storage Jump = Map Jump

makeWorld "World" [
    ''Position
  , ''Velocity
  , ''Acceleration
  , ''BoundingBox
  , ''Friction
  , ''Player
  , ''Collisions
  , ''Floor
  , ''Texture
  , ''GlobalTime
  , ''PhysicsTime
  , ''Gravity
  , ''Camera
  , ''Font
  , ''Jump
  ]

type System' a = System World a
