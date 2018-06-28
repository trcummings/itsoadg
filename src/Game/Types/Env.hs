{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Env where

import qualified SDL
import           Data.IORef

import           Game.Types.EventQueue (EventQueue(..))
import           Game.Types.GameState (RunState(..), GameState(..))

data Env = Env
  { envVideoConfig   :: VideoConfig
  , envRuntimeConfig :: RuntimeConfig
  , envGameState     :: !(IORef GameState) }

data DebugMode =
    DebugMode'DrawDebug
  | DebugMode'None

data RuntimeConfig = RuntimeConfig
  { rcDebugMode :: !(IORef DebugMode) }

data VideoConfig = VideoConfig
  { vcWindow     :: SDL.Window
  , vcRenderer   :: SDL.Renderer }
