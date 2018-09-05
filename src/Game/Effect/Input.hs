module Game.Effect.Input where

import qualified SDL
import           Linear (V2(..))
import qualified Data.Map as Map (lookup)
import           Apecs
import           Data.Map (mapWithKey, empty, fromList, insert, (!), (!?))
import           Data.Maybe (catMaybes)
import           Control.Lens ((&), (%~))
import           KeyState
  ( KeyState(..)
  , updateKeyState
  , maintainKeyState
  , initKeyState
  , ksCounter
  , isPressed
  , isTouched
  , isReleased
  , isHeld )

import           Game.World.TH (ECS)
import           Game.Util.Constants (frameDeltaSeconds)
import           Game.Types
  ( Inputs(..)
  , PlayerInput(..)
  , MousePosition(..) )

-- poll SDL for events, process them
processInputs :: ECS ()
processInputs = do
  events <- SDL.pollEvents
  inputs <- get global :: ECS Inputs
  set global $ consumeInputs events inputs

-- update each fixed step cycle
updateInputs :: ECS ()
updateInputs = do
  inputs <- get global :: ECS Inputs
  set global $ inputs { _keyboardInput = maintainInputs $ _keyboardInput inputs }

-- helper functions
updateKey :: KeyState Double -> SDL.InputMotion -> KeyState Double
updateKey ks motion = updateKeyState frameDeltaSeconds ks touched
  where touched = motion == SDL.Pressed

maintainKey :: KeyState Double -> KeyState Double
maintainKey ks = maintainKeyState frameDeltaSeconds ks

handleKeyboardEvent :: SDL.KeyboardEventData -> Inputs -> Inputs
handleKeyboardEvent keyboardEvent inputs =
  let keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
      motion  = SDL.keyboardEventKeyMotion keyboardEvent
      m       = _keyboardInput inputs
  -- NB: Int keys work best performance-wise for maps,
  --     if performance is slow here, change to Int map
  in case (Map.lookup keyCode $ _inputs m) of
      -- add to inputs & newly updated
      Just ks -> inputs { _keyboardInput =
        m { _inputs       = insert keyCode (updateKey ks motion) (_inputs m)
          , _justModified = insert keyCode True (_justModified m) }
      }
      Nothing -> inputs

handleMousePosEvent :: SDL.MouseMotionEventData -> Inputs -> Inputs
handleMousePosEvent mouseMotionEvent inputs =
  let (SDL.P pos) = SDL.mouseMotionEventPos mouseMotionEvent
  in inputs { _mousePosition = MousePosition pos }

handleSDLInput :: SDL.Event -> Inputs -> Inputs
handleSDLInput event inputs = case SDL.eventPayload event of
  -- keyboard presses
  SDL.KeyboardEvent    e -> handleKeyboardEvent e inputs
  -- mouse movements
  SDL.MouseMotionEvent e -> handleMousePosEvent e inputs
  -- otherwise
  _ -> inputs

getInputs :: ECS Inputs
getInputs = do
  inputs <- get global :: ECS Inputs
  return inputs

maintainInputs :: PlayerInput -> PlayerInput
maintainInputs m =
  m { _inputs       = mapWithKey maintainIfNotNew (_inputs m)
    , _justModified = empty }
  where maintainIfNotNew k v =
          case (_justModified m !? k) of Nothing -> maintainKey v
                                         Just _  -> v

consumeInputs :: [SDL.Event] -> Inputs -> Inputs
consumeInputs events inputs = foldr handleSDLInput inputs events
