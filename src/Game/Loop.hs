module Game.Loop where

import Apecs                (cmap, get, set, global, runGC, cmapM_)
import Control.Monad        (when)

import Game.Types           (SceneControl(..), Scene(..))
import Game.Effect.Renderer (clearScreen, drawScreen)
import Game.Effect.Clock
  ( accumulateFixedTime
  , clearFixedTime
  , getFixedTime )
import Game.Effect.Input    (processInputs, updateInputs)
import Game.Util.Constants  (dT)
import Game.World.TH        (ECS)

-- import Game.System.Audio (stepAudioQueue)

-- import Game.Scene.Title (stepTitle, initTitle, renderTitle, cleanUpTitle)
-- import Game.Scene.Play  (stepPlay, initPlay, renderPlay, cleanUpPlay)
--

-- update physics multiple times if time step is less than frame update time
innerStep :: Double -> Scene -> ECS ()
innerStep acc scene = do
  if (acc < dT)
  then return ()
  -- when we've accumulated a fixed step update
  else do
    -- update the inputs each fixed step
    updateInputs
    -- run the scene's step function
    step scene
    -- clear away the fixed time we've accumulated
    clearFixedTime
    -- get next fixed time for update
    (_, acc') <- getFixedTime
    -- recurse if we need to run another fixed step update
    innerStep acc' scene
  where
    step s = do
      case s of
        -- Scene'Title -> stepTitle
        -- Scene'Play  -> stepPlay
        _           -> return ()

stepSceneControl :: Scene -> Scene -> ECS ()
stepSceneControl scene nextScene = do
  when (nextScene /= scene) $ do
    case nextScene of
      -- Scene'Title -> initTitle
      Scene'Title -> return ()
      Scene'Play  -> do
        -- case scene of Scene'Title -> cleanUpTitle
        --               _           -> return ()
        -- initPlay
        return ()
      Scene'Quit  -> return ()
        -- case scene of Scene'Title -> cleanUpTitle
        --               Scene'Play  -> cleanUpPlay
        --               _           -> return ()
      _           -> return ()
    set global ( SceneControl { _scene     = nextScene
                              , _nextScene = nextScene } )
    return ()

mainLoop :: ECS ()
mainLoop = do
  -- prep screen for next render
  clearScreen
  -- update player input button-key keystate-value map
  processInputs
  -- accumulate fixed time for updates
  accumulateFixedTime
  -- get fixed time for inner step
  (_, acc) <- getFixedTime
  -- run inner step
  sc :: SceneControl <- get global
  innerStep acc $ _scene sc
  case _scene sc of
    Scene'Title -> return ()
    Scene'Play  -> return ()
    -- Scene'Title -> renderTitle
    -- Scene'Play  -> renderPlay
    _           -> return ()
  -- play audio
  -- stepAudioQueue
  -- run current render, swap background buffer
  drawScreen
  -- garbage collect. yes, every frame
  runGC
  -- loop if game still running
  stepSceneControl (_scene sc) (_nextScene sc)
  case _scene sc of
    Scene'Quit  -> return ()
    _           -> mainLoop
