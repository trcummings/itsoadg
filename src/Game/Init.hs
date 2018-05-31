{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Init where

import qualified SDL
import qualified Animate
import           SDL.Font  as TTF (free, load, blended)
import           SDL (($=))
import           Linear (V4(..), V2(..))
import           Data.Text (singleton)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Apecs (newEntity)

import           Game.Render (loadTexture, toTexture)
import           Game.World (System')
import           Game.Constants
  ( spriteSize
  , floorFriction
  , playerPos
  , initialSize
  , screenHeight
  , screenWidth )
import           Game.Types
  ( Seconds(..)
  , Position(..)
  , Velocity(..)
  , Acceleration(..)
  , BoundingBox(..)
  , Gravity(..)
  , Camera(..)
  , CameraTarget(..)
  , SpriteSheet(..)
  , Friction(..)
  , Font(..)
  , Jump(..)
  , Player(..), PlayerKey(..), PlayerAction(..)
  , AnimationKey(..) )
import           Game.Step (Step(..))
import           Game.Jump (floating)
import           Game.Sprite (loadSpriteSheet)

characters =
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ [' ', ':', ',', '-', '.']

type SpriteAnimation = System' (Animate.SpriteSheet AnimationKey SDL.Texture Seconds)

initSystems :: SDL.Renderer -> System' ()
initSystems renderer = void $ do
  -- load in assets, convert to textures
  spriteSheetTexture <- liftIO $ loadTexture renderer "assets/red_square.bmp"
  smallFont <- liftIO $ TTF.load "assets/04B_19__.TTF" 24
  fontMap <- liftIO $ mapM (\c -> do
        texture <- toTexture renderer =<< TTF.blended
          smallFont
          (V4 255 255 255 255)
          (singleton c)
        return (c, texture)
      ) $ characters
  -- after we convert our font to textures we dont need the resource anymore
  TTF.free smallFont

  -- load in player spritesheet
  playerSpriteSheet <- liftIO $
    loadSpriteSheet renderer "assets/player-32.json" :: SpriteAnimation

  -- entities
  player <- newEntity ( -- player
      Player (Step'Sustain PlayerAction'IdleRight)
    , ( Position $ V2 7 ((screenHeight / 2) - 2)
      , Velocity $ V2 0 0
      -- , Acceleration $ V2 0 0
      , BoundingBox $ V2 1 1.55
      , floating )
    , Gravity
    , SpriteSheet playerSpriteSheet (Animate.initPosition PlayerKey'RIdle)
    , spriteSheetTexture )

  newEntity ( -- camera
      Camera { size = V2 screenWidth screenHeight, ppos = V2 0 0 }
    , CameraTarget player
    , Position $ V2 7 ((screenHeight / 2) - 1)
    , Acceleration $ V2 0 0 )

  newEntity ( -- small font
      Position $ V2 0 0
    , Font fontMap )

  newEntity ( -- floor
      Position $ V2 0 (screenHeight - 1)
    -- , Friction floorFriction
    , BoundingBox (V2 screenWidth 1) )

  newEntity ( --floating platform
      Position $ V2 5 (screenHeight / 2)
    -- , Friction floorFriction
    , BoundingBox $ V2 6 1 )

  newEntity ( --floating platform 2
      Position $ V2 13 (screenHeight / 2)
    -- , Friction floorFriction
    , BoundingBox $ V2 6 1 )

  newEntity ( --wall
      Position $ V2 (screenWidth - 10) (screenHeight - 5)
    , BoundingBox $ V2 1 4 )

  newEntity ( --wall2
      Position $ V2 (screenWidth - 1) (screenHeight - 8)
    , BoundingBox $ V2 1 7 )
