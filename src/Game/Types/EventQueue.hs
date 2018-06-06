{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Game.Types.EventQueue where

import           Apecs (System, Entity, Has)

data QueueEvent =
  forall w c. Has w c => QueueEvent Entity (c -> c)

instance Show QueueEvent where
  show (QueueEvent e _) = "QueueEvent from " ++ show e

newtype EventQueue =
  EventQueue [QueueEvent]
  deriving Show
