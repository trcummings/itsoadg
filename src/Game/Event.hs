module Game.Event where

import qualified SDL
import           Apecs (cmap)
import           Data.Map (member, insert)
import           Control.Monad.IO.Class (liftIO)


import           Game.World (System')
import           Game.Types (PlayerInput(..), MousePosition(..))

handleEvent :: SDL.Event -> System' ()
handleEvent event = do
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      cmap $ \(PlayerInput m) ->
        if (member keyCode m)
        then PlayerInput $ insert keyCode motion m
        else PlayerInput m
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent

    SDL.MouseMotionEvent mouseMotionEvent -> do
      cmap $ \(MousePosition _) -> MousePosition pos
      where (SDL.P pos) = SDL.mouseMotionEventPos mouseMotionEvent

    _ -> return ()
