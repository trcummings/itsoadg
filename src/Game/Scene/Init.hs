module Game.Scene.Init where

import Apecs                  (newEntity, get, set, global)
import Control.Monad.IO.Class (liftIO)
import Control.Monad          (void)

import Game.Types       (Scene(..), SceneControl(..))
import Game.Util.Config (initConfig)
import Game.World.TH    (ECS)

initialize :: ECS ()
initialize = return ()

step :: ECS ()
step = do
  vc <- liftIO $ initConfig
  void $ newEntity vc
  sc <- get global :: ECS SceneControl
  set global $ sc { _nextScene = Scene'Title }

render :: ECS ()
render = return ()

cleanUp :: ECS ()
cleanUp = return ()
