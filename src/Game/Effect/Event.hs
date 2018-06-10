{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Event where

import           Control.Monad.State (MonadState(..), get, put)

import           Game.Types (EventQueue(..))

class Monad m => Event m where

-- clearScreen' :: (Event m, MonadState EventQueue m) => m ()
-- clearScreen' = undefined

-- drawScreen' :: (Event m, MonadState EventQueue m) => m ()
-- drawScreen' = undefined
