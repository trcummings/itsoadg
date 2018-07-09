{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.HasGameState where

import           Control.Monad.Reader (MonadReader, ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (readIORef, modifyIORef)

import           Game.World (Env)
import           Game.Types (GameEnv(..), GameState(..))

class Monad m => HasGameState m where
  getGameState :: m GameState
  setGameState :: (GameState -> GameState) -> m ()

getGameState' :: (HasGameState m, MonadReader Env m, MonadIO m) => m GameState
getGameState' = do
  gsIO <- envGameState <$> ask
  gs   <- liftIO $ readIORef gsIO
  return gs

setGameState' :: (HasGameState m, MonadReader Env m, MonadIO m)
              => (GameState -> GameState) -> m ()
setGameState' f = do
  gsIO <- envGameState <$> ask
  liftIO $ modifyIORef gsIO f
