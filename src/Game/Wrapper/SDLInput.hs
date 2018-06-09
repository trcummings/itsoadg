module Game.Wrapper.SDLInput where

import qualified SDL
import           Control.Monad.IO.Class (MonadIO(..))

class Monad m => SDLInput m where
  pollEvents :: m [SDL.Event]

pollEvents' :: MonadIO m => m [SDL.Event]
pollEvents' = SDL.pollEvents
