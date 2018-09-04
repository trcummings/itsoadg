module Game.Scene.Quit where

import Apecs (cmapM_)
import Control.Monad.IO.Class (liftIO)

import Game.World.TH (ECS)
import Game.Types (VideoConfig)
import Game.Util.Config (cleanUpConfig)

-- NB: initializing the Quit scene is actually a clean up function
initialize :: ECS ()
initialize = cmapM_ $ \(vc :: VideoConfig) -> liftIO $ cleanUpConfig vc

step :: ECS ()
step = return ()

render :: ECS ()
render = return ()

cleanUp :: ECS ()
cleanUp = return ()
