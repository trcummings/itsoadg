module Game.Types.SDLConfig where

import qualified SDL

data SDLConfig = SDLConfig
  { sdlWindow   :: SDL.Window
  , sdlRenderer :: SDL.Renderer }
