module Game.Wrapper.SDLTime where

import qualified SDL.Time as Time (ticks)
import           Control.Monad.IO.Class (liftIO, MonadIO(..))

class Monad m => SDLTime m where
   nextTick :: m Double

nextTick' :: MonadIO m => m Double
nextTick' = liftIO (fromIntegral <$> Time.ticks :: IO Double)
