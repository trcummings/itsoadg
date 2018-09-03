{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Env where

import qualified SDL
import           Data.IORef

import           Game.Types.GameState (GameState(..))

data VideoConfig = VideoConfig
  { _window    :: SDL.Window
  , _glContext :: SDL.GLContext }

-- data GameEnv w = GameEnv
data GameEnv = GameEnv
  { _videoConfig :: VideoConfig
  , _gameState   :: !(IORef GameState) }
  -- , _World       :: w } -- game world
