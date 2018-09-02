{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.World (World, Env, initWorld) where

import Apecs

import Game.Types
  ( GameEnv
  -- , Position
  -- , Velocity
  -- , Acceleration
  -- , BoundingBox
  , Camera(..)
  , Model(..)
  , Position3D(..)
  -- , CameraTarget
  -- , Texture
  -- , SpriteSheet
  -- , Gravity
  -- , Friction
  -- , Font
  -- , Jump(..)
  -- , Player(..)
  -- , SpriteSheet(..)
  -- , FlowMeter(..)
  -- , HardFlow(..)
  -- , FlowEffectEmitter(..)
  -- , SoundBank(..)
  -- , CollisionModule
  -- , Commandable
  -- , QueueEvent

  , HasOptionMenuEvent
  , ActiveOptionList
  , OptionList )

-- instance Component Position where
--   type Storage Position = Map Position
instance Component Position3D where
  type Storage Position3D = Map Position3D
--
-- instance Component Velocity where
--   type Storage Velocity = Map Velocity
--
-- instance Component Acceleration where
--   type Storage Acceleration = Map Acceleration
--
-- instance Component BoundingBox where
--   type Storage BoundingBox = Map BoundingBox
--
-- instance Component Player where
--   type Storage Player = Unique Player

instance Component Camera where
  type Storage Camera = Unique Camera

instance Component Model where
  type Storage Model = Map Model
--
-- instance Component CameraTarget where
--   type Storage CameraTarget = Unique CameraTarget
--
-- instance Component Texture where
--   type Storage Texture = Map Texture
--
-- instance Component SpriteSheet where
--   type Storage SpriteSheet = Map SpriteSheet
--
-- instance Component Gravity where
--   type Storage Gravity = Map Gravity
--
-- instance Component Friction where
--   type Storage Friction = Map Friction
--
-- instance Component Font where
--   type Storage Font = Map Font

-- instance Component Jump where
--   type Storage Jump = Map Jump
--
-- instance Component FlowMeter where
--   type Storage FlowMeter = Map FlowMeter
--
-- instance Component HardFlow where
--   type Storage HardFlow = Map HardFlow
--
-- instance Component FlowEffectEmitter where
--   type Storage FlowEffectEmitter = Map FlowEffectEmitter
--
-- instance Component SoundBank where
--   type Storage SoundBank = Unique SoundBank
--
-- instance Component CollisionModule where
--   type Storage CollisionModule = Map CollisionModule
--
-- instance Component Commandable where
--   type Storage Commandable = Map Commandable

instance Component OptionList where
  type Storage OptionList = Map OptionList

instance Component HasOptionMenuEvent where
  type Storage HasOptionMenuEvent = Map HasOptionMenuEvent

instance Component ActiveOptionList where
  type Storage ActiveOptionList = Map ActiveOptionList

makeWorld "World" [
    ''Position3D
  --   ''Position
  -- , ''Velocity
  -- , ''Acceleration
  -- , ''BoundingBox
  -- , ''Friction
  -- , ''Player
  -- , ''Texture
  -- , ''SpriteSheet
  -- , ''Gravity
  , ''Camera
  , ''Model
  -- , ''CameraTarget
  -- , ''Font
  -- , ''Jump
  -- , ''FlowMeter
  -- , ''HardFlow
  -- , ''FlowEffectEmitter
  -- , ''SoundBank
  -- , ''CollisionModule
  -- , ''Commandable
  , ''OptionList
  , ''HasOptionMenuEvent
  , ''ActiveOptionList
  ]

type Env = GameEnv World
