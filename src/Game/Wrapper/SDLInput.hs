module Game.Wrapper.SDLInput where

import qualified SDL
import           Control.Monad.IO.Class (MonadIO(..))

import           Game.Types (QueueEvent(..))

class Monad m => SDLInput m where
  pollEvents :: m [QueueEvent]

pollEvents' :: MonadIO m => m [QueueEvent]
pollEvents' = map InputEvent <$> SDL.pollEvents
