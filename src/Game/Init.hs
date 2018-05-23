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
import           SDL.Font as TTF (free, load, blended)
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
  ( Player(..)
  , Position(..)
  , Velocity(..)
  , Acceleration(..)
  , BoundingBox(..)
  , Gravity(..)
  , Camera(..)
  , CameraTarget(..)
  , Collisions(..)
  , Friction(..)
  , Floor(..)
  , Font(..)
  , Jump(..) )

characters =
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ [' ', ':', ',', '-', '.']

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

  -- entities
  player <- newEntity ( -- player
      Player
    , ( Position playerPos
      , Velocity $ V2 0 0
      , Acceleration $ V2 0 0
      , BoundingBox spriteSize
      , Jump { jumpCommandReceived = False, isJumping = False } )
    , Gravity
    , Collisions []
    , spriteSheetTexture )

  newEntity ( -- camera
      Camera { size = V2 screenWidth screenHeight, ppos = V2 0 0 }
    , CameraTarget player
    , Position playerPos
    , Acceleration $ V2 0 0 )

  newEntity ( -- small font
      Position $ V2 0 0
    , Font fontMap )

  newEntity ( -- floor
      Floor
    , Position $ V2 0 (screenHeight - 1)
    , Friction floorFriction
    , Collisions []
    , BoundingBox (V2 screenWidth 1) )
