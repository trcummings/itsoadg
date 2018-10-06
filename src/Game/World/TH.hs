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
import Codec.Wavefront (WavefrontOBJ)


import Game.World.History
import Game.Types
  ( VideoConfig

  , SceneControl
  , Inputs
  , Clock
  , PolygonMode
  , ProgramMap

  , Hierarchy

  , StaminaMeter
  , SanityMeter

  , Position3D
  , Orientation
  , HasMoveCommand

  , CollisionModule

  , Player
  , FloorCircle
  , Frustum

  , SimpleCube

  , Terrain
  , Billboard

  , Door
  , DoorPanel

  , BSPMap
  , DebugHUD
  , HUDType
  , FontMap
  , FontInfo

  , VAO
  , ShaderProgram
  , BufferResource
  , RotatingCube
  , Texture
  , ObjData
  , Renderable

  , SpriteSheet

  , Camera

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

instance Component PolygonMode where
  type Storage PolygonMode = Global PolygonMode

instance Component ProgramMap where
  type Storage ProgramMap = Global ProgramMap

instance Component FontMap where
  type Storage FontMap = Global FontMap


-- Entity related Components
instance Component Hierarchy where
  type Storage Hierarchy = Map Hierarchy

instance Component SanityMeter where
  type Storage SanityMeter = Global SanityMeter

instance Component StaminaMeter where
  type Storage StaminaMeter = Global StaminaMeter

instance Component Position3D where
  type Storage Position3D = History (Map Position3D)

instance Component Orientation where
  type Storage Orientation = Map Orientation

instance Component HasMoveCommand where
  type Storage HasMoveCommand = Map HasMoveCommand

instance Component CollisionModule where
  type Storage CollisionModule = Map CollisionModule

instance Component RotatingCube where
  type Storage RotatingCube = Map RotatingCube


-- Render System Types
instance Component ShaderProgram where
  type Storage ShaderProgram = Map ShaderProgram

instance Component BufferResource where
  type Storage BufferResource = Map BufferResource

instance Component Texture where
  type Storage Texture = Map Texture

instance Component WavefrontOBJ where
  type Storage WavefrontOBJ = Map WavefrontOBJ

instance Component Renderable where
  type Storage Renderable = Map Renderable

instance Component HUDType where
  type Storage HUDType = Map HUDType

instance Component FontInfo where
  type Storage FontInfo = Map FontInfo

instance Component SpriteSheet where
  type Storage SpriteSheet = Map SpriteSheet

instance Component VAO where
  type Storage VAO = Unique VAO



-- Render Entity Types
instance Component Player where
  type Storage Player = Unique Player

instance Component FloorCircle where
  type Storage FloorCircle = Map FloorCircle

instance Component Frustum where
  type Storage Frustum = Map Frustum

instance Component SimpleCube where
  type Storage SimpleCube = Map SimpleCube

instance Component Terrain where
  type Storage Terrain = Map Terrain

instance Component Billboard where
  type Storage Billboard = Map Billboard

instance Component Door where
  type Storage Door = Map Door

instance Component DoorPanel where
  type Storage DoorPanel = Map DoorPanel



instance Component BSPMap where
  type Storage BSPMap = Unique BSPMap

instance Component DebugHUD where
  type Storage DebugHUD = Unique DebugHUD




-- Camera Components
instance Component Camera where
  type Storage Camera = Unique Camera




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
  , ''PolygonMode
  , ''ProgramMap
  , ''FontMap

  , ''Hierarchy

  , ''SanityMeter
  , ''StaminaMeter

  , ''Position3D
  , ''Orientation
  , ''HasMoveCommand
  , ''CollisionModule
  , ''RotatingCube

  , ''Door
  , ''DoorPanel

  , ''ShaderProgram
  , ''Texture
  , ''WavefrontOBJ
  , ''Renderable
  , ''HUDType
  , ''FontInfo
  , ''VAO
  , ''BufferResource

  , ''SpriteSheet

  , ''BSPMap
  , ''Player
  , ''FloorCircle
  , ''Frustum
  , ''Billboard
  , ''Terrain
  , ''DebugHUD
  , ''SimpleCube

  , ''Camera

  , ''OptionList
  , ''HasOptionMenuEvent
  , ''ActiveOptionList
  ]

type ECS a = System World a
