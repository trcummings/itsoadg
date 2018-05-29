module Game.Event where

import qualified SDL
import           Apecs (cmap)
import qualified Data.Map as Map (lookup)
import           Data.Map (insert, (!))
import           Control.Monad.IO.Class (liftIO)
import           KeyState (KeyState(..), updateKeyState)

import           Game.World (System')
import           Game.Types (PlayerInput(..), MousePosition(..))
import           Game.Constants (frameDeltaSeconds)

updateKey :: KeyState Double -> SDL.InputMotion -> KeyState Double
updateKey ks motion = updateKeyState frameDeltaSeconds ks touched
  where touched = motion == SDL.Pressed

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
