{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Player where

import           Data.Text
import qualified Animate

-- data Step a
--   = Step'Change a a -- | Prev, Next
--   | Step'Sustain a
--   deriving (Show, Eq)

-- type Animations key = Animate.Animations key (Animate.SpriteClip key) Float
-- type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

data PlayerKey =
    PlayerKey'RWalk
  | PlayerKey'RJump
  | PlayerKey'RIdle
  | PlayerKey'LWalk
  | PlayerKey'LJump
  | PlayerKey'LIdle
  deriving (Show, Eq, Ord, Bounded, Enum)

-- data PlayerAction =
--     PlayerAction'MoveRight
--   | PlayerAction'Jump

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

-- stepPlayerFramePosition :: Step PlayerAction
--                         -> Animations PlayerKey
--                         -> Animate.Position PlayerKey Float
--                         -> Animate.Position PlayerKey Float
-- stepPlayerFramePosition (Step'Sustain _) animations pos =
--   Animate.stepPosition animations pos frameDeltaSeconds
-- stepPlayerFramePosition (Step'Change _ pA) _ _ = case pA of
