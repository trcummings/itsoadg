{-# LANGUAGE FlexibleContexts #-}

module Game.Wrapper.SDLRenderer where

import qualified SDL
import           SDL (($=))
import           Control.Monad.IO.Class (MonadIO(..))
import           Linear (V4(..))

class Monad m => SDLRenderer m where
  presentRenderer :: SDL.Renderer -> m ()
  clearRenderer   :: SDL.Renderer -> m ()

clearRenderer' :: MonadIO m => SDL.Renderer -> m ()
clearRenderer' renderer = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer

presentRenderer' :: MonadIO m => SDL.Renderer -> m ()
presentRenderer' = SDL.present
