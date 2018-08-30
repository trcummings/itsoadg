{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Env where

import qualified SDL
import           Data.IORef

import           Game.Types.GameState (GameState(..))

data VideoConfig = VideoConfig
  { vcWindow      :: SDL.Window
  , vcGLContext   :: SDL.GLContext }

data GameEnv w = GameEnv
  { envVideoConfig   :: VideoConfig
  , envGameState     :: !(IORef GameState)
  , envECSWorld      :: w } -- game world
