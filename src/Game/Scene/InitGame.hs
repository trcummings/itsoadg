module Game.Scene.InitGame where

import qualified SDL
import           Apecs
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)

import           Game.World.TH (ECS)
import           Game.Util.Config (initConfig, cleanUpConfig)
import           Game.Types (VideoConfig(..))

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
