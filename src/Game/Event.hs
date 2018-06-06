{-# LANGUAGE FlexibleContexts #-}

module Game.Event where

-- import           Apecs (Has, System, Component, Entity, get, set, global, cmap, modify)
-- import           Control.Monad.IO.Class (liftIO)

-- import           Game.Types (QueueEvent(..), EventQueue(..))
-- import           Game.World (System', World)

-- dispatchToEventQueue :: QueueEvent -> System' ()
-- dispatchToEventQueue qe = cmap $ \(EventQueue es) ->
--   EventQueue $ es ++ [qe]

-- runQueueEvent :: (Has World c, Component c) => Entity -> (c -> c) -> System' ()
-- runQueueEvent e f = modify e f

-- stepQueueEvents :: System' ()
-- stepQueueEvents = do
--   EventQueue events <- get global
--   mapM (\(QueueEvent e f) -> runQueueEvent e f) events
--   cmap $ \(EventQueue _) -> EventQueue []
