{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.System.Init where

import qualified SDL
import qualified SDL.Mixer as Mixer (load)
import qualified Animate
import           SDL.Font  as TTF (free, load, blended)
import           SDL (($=))
import           Linear (V4(..), V2(..))
import qualified Data.Map as Map (empty, fromList)
import           Data.Text (singleton)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Apecs (newEntity)

import           Game.Util.Render (loadTexture, toTexture)
import           Game.World (System')
-- import           Game.Util.Constants
--   ( spriteSize
--   , floorFriction
--   , playerPos
--   , initialSize
--   , screenHeight
--   , screenWidth
--   , initialJumpG
--   , initialFallG )
-- import           Game.Types
--   ( Seconds(..)
--   , Position(..)
--   , Velocity(..)
--   , Acceleration(..)
--   , BoundingBox(..)
--   , Gravity(..)
--   , Camera(..)
--   , CameraTarget(..)
--   , SpriteSheet(..)
--   , Friction(..)
--   , Font(..)
--   , Jump(..)
--   , Player(..), PlayerKey(..), PlayerAction(..)
--   , AnimationKey(..)
--   , FlowMeter(..)
--   , HardFlow(..)
--   , FlowEffectEmitter(..), FlowEffectEmitState(..)
--   , Step(..)
--   , SoundBank(..)
--   , Player'SFX'Key(..)
--   , CollisionModule(..), CollisionLayer(..)
--   , Commandable(..) )
-- import           Game.Sprite (loadSpriteSheet)

-- characters =
--      ['a'..'z']
--   ++ ['A'..'Z']
--   ++ ['0'..'9']
--   ++ [' ', ':', ',', '-', '.']


-- type SpriteAnimation = System' (Animate.SpriteSheet AnimationKey SDL.Texture Seconds)

initSystems :: SDL.Renderer -> System' ()
initSystems renderer = void $ return ()
-- initSystems renderer = void $ do
  -- load in assets, convert to textures
  -- spriteSheetTexture <- liftIO $ loadTexture renderer "assets/red_square.bmp"
  -- smallFont <- liftIO $ TTF.load "assets/fonts/04B_19__.TTF" 24
  -- fontMap <- liftIO $ mapM (\c -> do
  --       texture <- toTexture renderer =<< TTF.blended
  --         smallFont
  --         (V4 255 255 255 255)
  --         (singleton c)
  --       return (c, texture)
  --     ) $ characters
  -- -- after we convert our font to textures we dont need the resource anymore
  -- TTF.free smallFont

  -- -- load in player spritesheet
  -- playerSpriteSheet <- liftIO $
  --   loadSpriteSheet renderer "assets/sprites/player-32.json" :: SpriteAnimation

  -- -- load in sfx
  -- playerJump      <- Mixer.load "assets/sfx/sfx_movement_jump16.wav"
  -- playerLand      <- Mixer.load "assets/sfx/sfx_movement_jump17_landing.wav"
  -- playerBurnStart <- Mixer.load "assets/sfx/sfx_sounds_interaction16.wav"
  -- playerBurnLoop  <- Mixer.load "assets/sfx/sfx_sounds_interaction8.wav"
  -- playerBurnEnd   <- Mixer.load "assets/sfx/sfx_sounds_interaction9.wav"
  -- hardFlowEmit    <- Mixer.load

  -- entities
  -- player <- newEntity ( -- player
  --     Player (Step'Sustain PlayerAction'IdleRight)
  --   , Commandable
  --   , ( Position $ V2 7.5 ((screenHeight / 2) - 2.55)
  --     , Velocity $ V2 0 0
  --     -- , Acceleration $ V2 0 0
  --     , BoundingBox $ V2 1 1.55
  --     , CollisionModule { layer = CL'Player
  --                       , layerCollisions = [] }
  --     , Gravity { ascent  = initialJumpG
  --               , descent = initialFallG }
  --     , Jump { requested = False
  --            , onGround  = False } )
  --   , ( FlowMeter
  --         { currentFlow = 10
  --         , baseFlow    = 20
  --         , flowLimit   = 50
  --         , counter     = 0  }
  --     , FlowEffectEmitter NotEmittingFlowEffect )
  --   , SpriteSheet playerSpriteSheet (Animate.initPosition PlayerKey'RIdle) )

  -- newEntity ( -- camera
  --     Camera { size = V2 screenWidth screenHeight, ppos = V2 0 0 }
  --   , CameraTarget player
  --   , Position $ V2 7 ((screenHeight / 2) - 1)
  --   , Acceleration $ V2 0 0 )

  -- newEntity ( -- small font
  --     Position $ V2 0 0
  --   , Font fontMap )

  -- newEntity ( -- floor
  --     Position $ V2 0 (screenHeight - 1)
  --   -- , Friction floorFriction
  --   , CollisionModule CL'Surface
  --   , BoundingBox (V2 screenWidth 1) )

  -- newEntity ( --floating platform
  --     Position $ V2 5 (screenHeight / 2)
  --   -- , Friction floorFriction
  --   , CollisionModule CL'Surface
  --   , BoundingBox $ V2 6 1 )

  -- newEntity ( --floating platform 2
  --     Position $ V2 13 (screenHeight / 2)
  --   -- , Friction floorFriction
  --   , CollisionModule CL'Surface
  --   , BoundingBox $ V2 6 1 )

  -- newEntity ( --wall
  --     Position $ V2 (screenWidth - 10) (screenHeight - 5)
  --   , CollisionModule CL'Surface
  --   , BoundingBox $ V2 1 4 )

  -- newEntity ( --wall2
  --     Position $ V2 (screenWidth - 1) (screenHeight - 8)
  --   , CollisionModule CL'Surface
  --   , BoundingBox $ V2 1 7 )

  -- newEntity ( --hard flow1
  --     HardFlow
  --   , Gravity
  --       { ascent  = initialJumpG
  --       , descent = initialFallG }
  --   , Position $ V2 2 (screenHeight - 1.5)
  --   , CollisionModule { layer = CL'Collectible
  --                     , layerCollisions = [] }
  --   , BoundingBox $ V2 0.25 0.25
  --   , Velocity $ V2 0 0 )

  -- newEntity ( -- audio player
  --     SoundBank { bank =  Map.fromList [
  --                     ( Player'SFX'Jump     , playerJump      )
  --                   , ( Player'SFX'Land     , playerLand      )
  --                   -- , ( Player'SFX'BurnStart, playerBurnStart )
  --                   -- , ( Player'SFX'BurnLoop , playerBurnLoop  )
  --                   -- , ( Player'SFX'BurnEnd  , playerBurnEnd   )
  --                   ]
  --               , channelMap = Map.fromList [] } )
