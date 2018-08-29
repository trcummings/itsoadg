{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Env where

import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import           Data.IORef

import           Game.Types.EventQueue (EventQueue(..))
import           Game.Types.GameState (GameState(..))

data GameEnv w = GameEnv
  { envVideoConfig   :: VideoConfig
  , envRuntimeConfig :: RuntimeConfig
  , envGameState     :: !(IORef GameState)
  , envECSWorld      :: w } -- game world

data DebugMode =
    DebugMode'DrawDebug
  | DebugMode'None

data RuntimeConfig = RuntimeConfig
  { rcDebugMode :: !(IORef DebugMode) }

data VideoConfig = VideoConfig
  { vcWindow      :: SDL.Window
  , vcGLContext   :: SDL.GLContext
  , vcGLResources :: (GL.Program, GL.AttribLocation) }
