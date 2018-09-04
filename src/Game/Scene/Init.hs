module Game.Scene.Init where

import Apecs (newEntity)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Game.World.TH (ECS)
import Game.Util.Config (initConfig)

initialize :: ECS ()
initialize = return ()

step :: ECS ()
step = return ()

render :: ECS ()
render = return ()

cleanUp :: ECS ()
cleanUp = do
  vc <- liftIO $ initConfig
  void $ newEntity vc
