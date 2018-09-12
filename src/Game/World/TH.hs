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
  , Inputs
  , Clock

  , Position3D
  , Orientation

  , Model
  , VAO
  , Player
  , BSPMap

  , ShaderProgram
  , TexResource
  , BBResource
  , RotatingCube

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

instance Component Inputs where
  type Storage Inputs = Global Inputs

instance Component Clock where
  type Storage Clock = Global Clock


-- Entity related Components
instance Component Position3D where
  type Storage Position3D = Map Position3D

instance Component Orientation where
  type Storage Orientation = Map Orientation


instance Component ShaderProgram where
  type Storage ShaderProgram = Map ShaderProgram

instance Component Model where
  type Storage Model = Map Model

instance Component TexResource where
  type Storage TexResource = Map TexResource

instance Component BBResource where
  type Storage BBResource = Map BBResource


instance Component RotatingCube where
  type Storage RotatingCube = Map RotatingCube

instance Component VAO where
  type Storage VAO = Unique VAO


instance Component Player where
  type Storage Player = Unique Player

instance Component BSPMap where
  type Storage BSPMap = Unique BSPMap


-- Camera Components
instance Component Camera where
  type Storage Camera = Unique Camera

instance Component HasCameraEvent where
  type Storage HasCameraEvent = Unique HasCameraEvent


-- Option Menu Components
instance Component OptionList where
  type Storage OptionList = Map OptionList

instance Component HasOptionMenuEvent where
  type Storage HasOptionMenuEvent = Map HasOptionMenuEvent

instance Component ActiveOptionList where
  type Storage ActiveOptionList = Map ActiveOptionList

makeWorld "World" [
    ''VideoConfig

  , ''SceneControl
  , ''Inputs
  , ''Clock

  , ''Position3D
  , ''Orientation

  , ''Model
  , ''ShaderProgram
  , ''TexResource
  , ''BBResource
  , ''RotatingCube
  , ''VAO
  , ''BSPMap
  , ''Player

  , ''Camera
  , ''HasCameraEvent

  , ''OptionList
  , ''HasOptionMenuEvent
  , ''ActiveOptionList
  ]

type ECS a = System World a
