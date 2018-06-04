module Game.Event where

import qualified SDL
import           Apecs (cmap)
import qualified Data.Map as Map (lookup, map)
import           Data.Map (insert, (!))
import           Control.Monad.IO.Class (liftIO)
import           KeyState (KeyState(..), updateKeyState, maintainKeyState)

import           Game.World (System')
import           Game.Types (PlayerInput(..), MousePosition(..))
import           Game.Constants (frameDeltaSeconds)

updateKey :: KeyState Double -> SDL.InputMotion -> KeyState Double
updateKey ks motion = updateKeyState frameDeltaSeconds ks touched
  where touched = motion == SDL.Pressed

maintainKey :: KeyState Double -> KeyState Double
maintainKey ks = maintainKeyState frameDeltaSeconds ks

handleEvent :: SDL.Event -> System' ()
handleEvent event = do
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      cmap $ \(PlayerInput m) ->
        case (Map.lookup keyCode m) of
          -- NB: Int keys work best performance-wise for maps,
          --     if performance is slow here, change to Int map
          Just ks -> PlayerInput $ insert keyCode (updateKey ks motion) m
          Nothing -> PlayerInput m
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent

    SDL.MouseMotionEvent mouseMotionEvent -> do
      cmap $ \(MousePosition _) -> MousePosition pos
      where (SDL.P pos) = SDL.mouseMotionEventPos mouseMotionEvent

    _ -> return ()

maintainAllInputs :: System' ()
maintainAllInputs = cmap $ \(PlayerInput m) -> PlayerInput $ Map.map maintainKey m
