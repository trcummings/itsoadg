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

import           Apecs
import qualified Data.Map as Map (fromList)
import qualified SDL

import Game.Types
  ( Position
  , Velocity
  , Acceleration
  , BoundingBox
  , Player
  , Camera
  , CameraTarget
  , Texture
  , Gravity
  , Friction
  , Font
  , PhysicsTime(..)
  , GlobalTime(..)
  , PlayerInput(..)
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

instance Component CameraTarget where
  type Storage CameraTarget = Unique CameraTarget

instance Component Texture where
  type Storage Texture = Map Texture

instance Component Gravity where
  type Storage Gravity = Map Gravity

instance Component Friction where
  type Storage Friction = Map Friction

instance Component Font where
  type Storage Font = Map Font

instance Monoid PhysicsTime where
  mempty = PhysicsTime 0 0
instance Component PhysicsTime where
  type Storage PhysicsTime = Global PhysicsTime

instance Monoid GlobalTime where
  mempty = GlobalTime 0
instance Component GlobalTime where
  type Storage GlobalTime = Global GlobalTime

instance Monoid PlayerInput where
  mempty = PlayerInput $ Map.fromList [
      (SDL.KeycodeA, SDL.Released)
    , (SDL.KeycodeD, SDL.Released)
    , (SDL.KeycodeW, SDL.Released)
    ]
instance Component PlayerInput where
  type Storage PlayerInput = Global PlayerInput

instance Component Jump where
  type Storage Jump = Map Jump

makeWorld "World" [
    ''Position
  , ''Velocity
  , ''Acceleration
  , ''BoundingBox
  , ''Friction
  , ''Player
  , ''Texture
  , ''GlobalTime
  , ''PhysicsTime
  , ''PlayerInput
  , ''Gravity
  , ''Camera
  , ''CameraTarget
  , ''Font
  , ''Jump
  ]

type System' a = System World a
