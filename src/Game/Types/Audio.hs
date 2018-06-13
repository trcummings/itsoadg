{-# LANGUAGE TypeFamilies #-}

module Game.Types.Audio where

import           Apecs (Entity)

import           Game.Types.Player (Player'SFX'Key(..))

data Audio'Command =
    Audio'Halt
  | Audio'PlayOrSustain
  deriving Show

type AudioEvent = (Entity, Player'SFX'Key, Audio'Command)


