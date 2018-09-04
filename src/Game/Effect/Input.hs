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
  ipts   <- get global :: ECS Inputs
  set global $ consumeInputs events ipts

-- update each fixed step cycle
updateInputs :: ECS ()
updateInputs = do
  ipts <- get global :: ECS Inputs
  set global $ ipts { _keyboardInput = maintainInputs $ _keyboardInput ipts }

-- helper functions
updateKey :: KeyState Double -> SDL.InputMotion -> KeyState Double
updateKey ks motion = updateKeyState frameDeltaSeconds ks touched
  where touched = motion == SDL.Pressed

maintainKey :: KeyState Double -> KeyState Double
maintainKey ks = maintainKeyState frameDeltaSeconds ks

handleKeyboardEvent :: SDL.KeyboardEventData -> Inputs -> Inputs
handleKeyboardEvent keyboardEvent ipts =
  let keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
      motion  = SDL.keyboardEventKeyMotion keyboardEvent
      m       = _keyboardInput ipts
  -- NB: Int keys work best performance-wise for maps,
  --     if performance is slow here, change to Int map
  in case (Map.lookup keyCode $ inputs m) of
      -- add to inputs & newly updated
      Just ks -> ipts { _keyboardInput =
        m { inputs       = insert keyCode (updateKey ks motion) (inputs m)
          , justModified = insert keyCode True (justModified m) }
      }
      Nothing -> ipts

handleMousePosEvent :: SDL.MouseMotionEventData -> Inputs -> Inputs
handleMousePosEvent mouseMotionEvent ipts =
  let (SDL.P pos) = SDL.mouseMotionEventPos mouseMotionEvent
  in ipts { _mousePosition = MousePosition pos }

handleSDLInput :: SDL.Event -> Inputs -> Inputs
handleSDLInput event ipts = case SDL.eventPayload event of
  -- keyboard presses
  SDL.KeyboardEvent    e -> handleKeyboardEvent e ipts
  -- mouse movements
  SDL.MouseMotionEvent e -> handleMousePosEvent e ipts
  -- otherwise
  _ -> ipts

getInputs :: ECS Inputs
getInputs = do
  inputs <- get global :: ECS Inputs
  return inputs

maintainInputs :: PlayerInput -> PlayerInput
maintainInputs m =
  m { inputs       = mapWithKey maintainIfNotNew (inputs m)
    , justModified = empty }
  where maintainIfNotNew k v =
          case (justModified m !? k) of Nothing -> maintainKey v
                                        Just _  -> v

consumeInputs :: [SDL.Event] -> Inputs -> Inputs
consumeInputs events ipts = foldr handleSDLInput ipts events
