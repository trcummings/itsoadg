{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.Player where

import qualified Animate
import           Data.Text

data PlayerKey =
    PlayerKey'RWalk
  | PlayerKey'RJump
  | PlayerKey'RIdle
  | PlayerKey'LWalk
  | PlayerKey'LJump
  | PlayerKey'LIdle
  deriving (Show, Eq, Ord, Bounded, Enum)

data Player'SFX'Key =
    Player'SFX'Jump
  | Player'SFX'Land
  deriving (Show, Eq, Ord, Bounded, Enum)

data PlayerAction =
    PlayerAction'MoveRight
  | PlayerAction'JumpRight
  | PlayerAction'IdleRight
  | PlayerAction'MoveLeft
  | PlayerAction'JumpLeft
  | PlayerAction'IdleLeft
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