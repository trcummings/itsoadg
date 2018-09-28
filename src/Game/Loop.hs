module Game.Loop where

import Apecs                (runGC)

import Game.World.TH        (ECS)
import Game.Util.Constants  (dT)
import Game.Effect.Input    (processInputs, updateInputs)
import Game.Types           (SceneControl(..), Scene(..))
import Game.Effect.Renderer (clearScreen, drawScreen)
import Game.Effect.Clock
  ( accumulateFixedTime
  , clearFixedTime
  , getAccumulatedTime )
import Game.Effect.Scene
  ( SceneAction(..)
  , currentSceneAction
  , stepSceneControl
  , ifNotQuitting )

-- import Game.System.Audio (stepAudioQueue)

-- update physics multiple times if time step is less than frame update time
innerStep :: Double -> ECS ()
innerStep acc = do
  if (acc < dT)
  then return ()
  -- when we've accumulated a fixed step update
  else do
    -- update the inputs each fixed step
    updateInputs
    -- run the scene's step function
    currentSceneAction Step
    -- clear away the fixed time we've accumulated
    clearFixedTime
    -- recurse if we need to run another fixed step update
    getAccumulatedTime >>= innerStep

mainLoop :: ECS ()
mainLoop = do
  -- prep screen for next render
  clearScreen
  -- update player input button-key keystate-value map
  processInputs
  -- accumulate fixed time for updates
  accumulateFixedTime
  -- get fixed time for inner step and run inner step
  getAccumulatedTime >>= innerStep
  -- render scene
  currentSceneAction Render
  -- play audio
  -- stepAudioQueue
  -- run current render, swap background buffer
  drawScreen
  -- garbage collect. yes, every frame
  runGC
  -- decide if a scene transition needs to happen
  stepSceneControl
  -- loop if game still running
  -- NB: thunk buildup happening here, need to do something about it
  ifNotQuitting mainLoop
