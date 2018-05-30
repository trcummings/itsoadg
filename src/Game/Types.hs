{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types where

import qualified SDL (Texture, Keycode)
import qualified Data.Map as Map (Map)
import qualified Animate
import           Foreign.C.Types (CInt)
import           GHC.Int (Int32(..))
import           Linear (V2)
import           Apecs (Entity)
import           Data.Aeson (FromJSON(..), ToJSON(..))
import           KeyState
import           Data.Text


-- Utility types
newtype Unit =
  Unit Double
  deriving (Eq, Ord, Show, Num, Fractional)

newtype Seconds =
  Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)


-- Entity State Types
-- -- Player
data PlayerKey =
    PlayerKey'RWalk
  | PlayerKey'RJump
  | PlayerKey'RIdle
  | PlayerKey'LWalk
  | PlayerKey'LJump
  | PlayerKey'LIdle
  deriving (Show, Eq, Ord, Bounded, Enum)

data PlayerAction =
    PlayerAction'MoveRight
  | PlayerAction'JumpRight
  | PlayerAction'IdleRight
  | PlayerAction'MoveLeft
  | PlayerAction'JumpLeft
  | Playeraction'IdleLeft
  deriving (Show, Eq)

instance Animate.KeyName PlayerKey where
  keyName = playerKey'keyName

playerKey'keyName :: PlayerKey -> Text
playerKey'keyName = \case
  PlayerKey'RWalk -> "RWalk"
  PlayerKey'RJump -> "RJump"
  PlayerKey'RIdle -> "RIdle"
  PlayerKey'LWalk -> "LWalk"
  PlayerKey'LJump -> "LJump"
  PlayerKey'LIdle -> "LIdle"



-- Component types
newtype Position =
  Position (V2 Unit) -- center point
  deriving Show

newtype Velocity =
  Velocity (V2 Unit)
  deriving Show

newtype Acceleration =
  Acceleration (V2 Unit)
  deriving Show

newtype BoundingBox =
  BoundingBox (V2 Unit)
  deriving Show

data Player =
  Player PlayerAction
  deriving Show


data Camera = Camera
  { size :: (V2 Unit)   -- camera height and width
  , ppos :: (V2 Unit) } -- past position for verlet transform
  deriving Show

data CameraTarget =
  CameraTarget Entity
  deriving Show

data Texture =
  Texture SDL.Texture (V2 CInt)

type AnimationKey =
  PlayerKey

type Animations =
  Animate.Animations AnimationKey (Animate.SpriteClip AnimationKey) Seconds

newtype SpriteSheet =
  SpriteSheet (Animate.SpriteSheet AnimationKey SDL.Texture Seconds)

data Gravity = Gravity

newtype Friction =
  Friction Double
  deriving Show

data Font = Font [(Char, Texture)]

-- accumulator for physics frame time updates
data PhysicsTime = PhysicsTime
  { time  :: Double
  , accum :: Double }
  deriving Show

-- global timer
newtype GlobalTime =
  GlobalTime Double
  deriving Show

-- global input for player
data PlayerInput =
  PlayerInput (Map.Map SDL.Keycode (KeyState Double))
  deriving Show

data MousePosition =
  MousePosition (V2 Int32)

data Jump = Jump
  { buttonPressed :: Bool
  , isJumping     :: Bool
  , isGrounded    :: Bool }
  deriving (Eq, Show)
