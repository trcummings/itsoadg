{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.World.TH (ECS, World, initWorld) where

import Apecs

import Game.Types
  ( VideoConfig

  , SceneControl

  , Position3D

  , Model

  , Camera
  , HasCameraEvent

  , HasOptionMenuEvent
  , ActiveOptionList
  , OptionList )

-- Config Components
instance Component VideoConfig where
  type Storage VideoConfig = Unique VideoConfig


-- Globals
instance Component SceneControl where
  type Storage SceneControl = Global SceneControl


-- Entity related Components
instance Component Position3D where
  type Storage Position3D = Map Position3D


instance Component Model where
  type Storage Model = Map Model


instance Component Camera where
  type Storage Camera = Unique Camera

instance Component HasCameraEvent where
  type Storage HasCameraEvent = Unique HasCameraEvent


instance Component OptionList where
  type Storage OptionList = Map OptionList

instance Component HasOptionMenuEvent where
  type Storage HasOptionMenuEvent = Map HasOptionMenuEvent

instance Component ActiveOptionList where
  type Storage ActiveOptionList = Map ActiveOptionList

makeWorld "World" [
    ''VideoConfig

  , ''SceneControl

  , ''Position3D

  , ''Model

  , ''Camera
  , ''HasCameraEvent

  , ''OptionList
  , ''HasOptionMenuEvent
  , ''ActiveOptionList
  ]

type ECS a = System World a
