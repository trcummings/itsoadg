{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Env where

import qualified SDL
import           Data.IORef

import           Game.Types.GameState (GameState(..))

data VideoConfig = VideoConfig
  { _Window      :: SDL.Window
  , _GLContext   :: SDL.GLContext }

data GameEnv w = GameEnv
  { _VideoConfig   :: VideoConfig
  , _GameState     :: !(IORef GameState)
  , _World      :: w } -- game world
