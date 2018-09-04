module Game.System.Scene where

import Game.Scene.Title (stepTitle, initTitle, renderTitle, cleanUpTitle)
import Game.Scene.Play  (stepPlay, initPlay, renderPlay, cleanUpPlay)
import Game.World.TH (ECS)
import Game.Types (SceneControl(..), Scene(..))

data SceneAction =
    Init
  | Step
  | Render
  | CleanUp
