{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.SceneManager where

import Control.Monad (when)

import Game.Types (GameState(..), Scene(..))
import Game.Effect.HasGameState (HasGameState(..))

class Monad m => SceneManager m where
  getScene     :: m Scene
  setScene     :: Scene -> m ()
  getNextScene :: m Scene
  setNextScene :: Scene -> m ()

getScene' :: HasGameState m => m Scene
getScene' = _Scene <$> getGameState

setScene' :: HasGameState m => Scene -> m ()
setScene' scene = setGameState $ \gs -> gs { _Scene = scene }

getNextScene' :: HasGameState m => m Scene
getNextScene' = _NextScene <$> getGameState

setNextScene' :: HasGameState m => Scene -> m ()
setNextScene' scene = setGameState $ \gs -> gs { _NextScene = scene }
