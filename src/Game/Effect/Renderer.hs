{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Renderer where

-- import qualified SDL
--
-- import           Game.Types (VideoConfig(..))
-- import           Game.Wrapper.SDLRenderer
--   ( SDLRenderer
--   , clearRenderer
--   , presentRenderer )
-- import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
--
-- class Monad m => Renderer m where
--   clearScreen :: m ()
--   drawScreen  :: m ()
--
-- clearScreen' :: (SDLRenderer m, HasVideoConfig m) => m ()
-- clearScreen' = do
--   renderer <- vcRenderer <$> getVideoConfig
--   clearRenderer renderer
--
-- drawScreen' :: (SDLRenderer m, HasVideoConfig m) => m ()
-- drawScreen' = do
--   renderer <- vcRenderer <$> getVideoConfig
--   presentRenderer renderer
