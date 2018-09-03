{-# LANGUAGE ScopedTypeVariables #-}

module Game (main) where

import qualified SDL
import           Apecs
import           Control.Monad (void)
import           Control.Monad.IO.Class  (liftIO)

import           Game.World.TH (ECS, World, initWorld)
import           Game.Util.Config (initConfig, cleanUpConfig)
import           Game.Types (VideoConfig(..))
import           Game.Loop (mainLoop)

initVC :: ECS ()
initVC = do
  vc <- liftIO $ initConfig
  void $ newEntity vc

cleanUpVC :: VideoConfig -> IO ()
cleanUpVC vc = do
  dims <- SDL.get $ SDL.windowSize $ _window vc
  liftIO $ putStrLn $ show dims
  liftIO $ cleanUpVC vc
  return ()

main :: IO ()
main = initWorld >>= runSystem mainLoop
