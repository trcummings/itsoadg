{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Renderer where

import qualified SDL
import           Control.Monad.Reader (MonadReader(..), ask)

import           Game.Types (SDLConfig(..))
import           Game.Wrapper.SDLRenderer
  ( SDLRenderer
  , clearRenderer
  , presentRenderer )

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen  :: m ()

clearScreen' :: (SDLRenderer m, MonadReader SDLConfig m) => m ()
clearScreen' = do
  renderer <- sdlRenderer <$> ask
  clearRenderer renderer

drawScreen' :: (SDLRenderer m, MonadReader SDLConfig m) => m ()
drawScreen' = do
  renderer <- sdlRenderer <$> ask
  presentRenderer renderer
