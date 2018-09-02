module Game.Effect.Input where

import qualified SDL
import           Linear (V2(..))
import qualified Data.Map as Map (lookup)
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

import           Game.Effect.HasGameState (HasGameState(..))
import           Game.Wrapper.SDLInput (SDLInput(..))
import           Game.Util.Constants (frameDeltaSeconds)
import           Game.Types
  ( GameState(..)
  , PlayerInput(..)
  , MousePosition(..) )

class Monad m => Input m where
  processInputs :: m ()
  updateInputs  :: m ()
  getInputs     :: m PlayerInput

-- helper functions
updateKey :: KeyState Double -> SDL.InputMotion -> KeyState Double
updateKey ks motion = updateKeyState frameDeltaSeconds ks touched
  where touched = motion == SDL.Pressed

maintainKey :: KeyState Double -> KeyState Double
maintainKey ks = maintainKeyState frameDeltaSeconds ks

handleSDLInput :: SDL.Event -> GameState -> GameState
handleSDLInput event gs =
  -- keyboard presses
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      let keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
          motion  = SDL.keyboardEventKeyMotion keyboardEvent
          m       = _PlayerInput gs
      -- NB: Int keys work best performance-wise for maps,
      --     if performance is slow here, change to Int map
      in case (Map.lookup keyCode $ inputs m) of
          -- add to inputs & newly updated
          Just ks -> gs { _PlayerInput =
            m { inputs     = insert keyCode (updateKey ks motion) (inputs m)
            , justModified = insert keyCode True (justModified m) }
          }
          Nothing -> gs

    -- mouse movements
    SDL.MouseMotionEvent mouseMotionEvent ->
      let (SDL.P pos) = SDL.mouseMotionEventPos mouseMotionEvent
      in gs { _MousePosition = MousePosition pos }

    _ -> gs

maintainInputs :: PlayerInput -> PlayerInput
maintainInputs m =
  m { inputs     = mapWithKey maintainIfNotNew (inputs m)
  , justModified = empty }
  where maintainIfNotNew k v =
          case (justModified m !? k) of Nothing -> maintainKey v
                                        Just _  -> v

-- class functions

processInputs' :: (HasGameState m, SDLInput m) => m ()
processInputs' = do
  events <- pollEvents
  setGameState $ \gs -> foldr handleSDLInput gs events

updateInputs' :: (HasGameState m, SDLInput m) => m ()
updateInputs' = setGameState $ \gs ->
  gs { _PlayerInput = maintainInputs $ _PlayerInput gs }

getInputs' :: HasGameState m => m PlayerInput
getInputs' = _PlayerInput <$> getGameState
