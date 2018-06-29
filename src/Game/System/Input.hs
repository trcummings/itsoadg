module Game.System.Input where

import qualified SDL
import           Apecs (Entity, global, get, set)
import qualified Apecs as A (cmap)
import qualified Data.Map as Map (lookup, mapWithKey, empty)
import           Data.Map (insert, (!), (!?))
import           Data.Maybe (catMaybes)
import           Control.Monad.IO.Class (liftIO)
import           KeyState
  ( KeyState(..)
  , updateKeyState
  , maintainKeyState
  , ksCounter
  , isPressed
  , isTouched
  , isReleased
  , isHeld )

import           Game.Wrapper.Apecs (Apecs(..), emap)
import           Game.Wrapper.SDLInput (SDLInput(..))
import           Game.World (System', SystemFn)
import           Game.Types
  ( PlayerInput(..)
  , MousePosition(..)
  , QueueEvent(..)
  , Commandable
  , Player
  , To(..)
  , From(..)
  , Dir(..)
  , Motion(..)
  , MovementCommand(..) )
import           Game.Util.Constants (frameDeltaSeconds)

updateKey :: KeyState Double -> SDL.InputMotion -> KeyState Double
updateKey ks motion = updateKeyState frameDeltaSeconds ks touched
  where touched = motion == SDL.Pressed

maintainKey :: KeyState Double -> KeyState Double
maintainKey ks = maintainKeyState frameDeltaSeconds ks

stepSDLInput :: (Apecs m, SDLInput m) => m ()
stepSDLInput = do
  sdlEvents  <- pollEvents
  mapM handleSDLInput sdlEvents
  return ()

handleSDLInput :: (Apecs m) => QueueEvent -> m ()
handleSDLInput (InputEvent event) = do
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      cmap $ \(m@(PlayerInput _ _)) ->
        case (Map.lookup keyCode $ inputs m) of
          -- NB: Int keys work best performance-wise for maps,
          --     if performance is slow here, change to Int map
          Just ks ->
            -- add to inputs & newly updated
            m { inputs       = insert keyCode (updateKey ks motion) (inputs m)
              , justModified = insert keyCode True (justModified m) }
          Nothing -> m
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent

    SDL.MouseMotionEvent mouseMotionEvent -> do
      cmap $ \(MousePosition _) -> MousePosition pos
      where (SDL.P pos) = SDL.mouseMotionEventPos mouseMotionEvent

    _ -> return ()
handleSDLInput _ = return ()


maintainInputs :: PlayerInput -> PlayerInput
maintainInputs m =
  m { inputs = Map.mapWithKey maintainIfNotNew (inputs m) }
  where maintainIfNotNew k v =
          case (justModified m !? k) of Nothing -> maintainKey v
                                        Just _  -> v

-- if W tapped then jump, but not if held
addJumpCommand :: PlayerInput -> Entity -> Maybe QueueEvent
addJumpCommand (PlayerInput m _) e =
  let toEvent p = Just $ CommandSystemEvent (To e, From e, Command'Jump p)
      wPress    = m ! SDL.KeycodeW
  in if (KeyState.isPressed wPress)
     then if (KeyState.isReleased wPress)
          then toEvent Released
          else toEvent Pressed
     else Nothing

-- if both A & D pressed, pick which ever one was held the least amount
addMoveCommand :: PlayerInput -> Entity -> Maybe QueueEvent
addMoveCommand (PlayerInput m _) e =
  let leftPress    = m ! SDL.KeycodeA
      rightPress   = m ! SDL.KeycodeD
      leftCount    = ksCounter $ m ! SDL.KeycodeA
      rightCount   = ksCounter $ m ! SDL.KeycodeD
      toEvent d = Just $ CommandSystemEvent (To e, From e, Command'Move $ d)
  -- if pressing both buttons at the same time
  in if (KeyState.isTouched leftPress && KeyState.isTouched rightPress)
     -- if theyre pressed at EXACTLY the same frame
     then if (leftCount == Nothing && rightCount == Nothing)
          then Nothing
          else case leftCount of
                 Nothing -> toEvent $ Just L
                 Just lc -> case rightCount of
                   Nothing -> toEvent $ Just R
                   Just rc -> if (lc < rc)
                              then toEvent $ Just L
                              else toEvent $ Just R
     else if (KeyState.isTouched leftPress)
          then toEvent $ Just L
          else if (KeyState.isTouched rightPress)
               then toEvent $ Just R
               -- if neither touched, stop movement event
               else toEvent $ Nothing

stepPlayerCommands :: PlayerInput
                   -> (Player, Commandable, Entity)
                   -> ((Player, Commandable), [QueueEvent])
stepPlayerCommands m (p, c, e) =
  ( (p, c)
  , catMaybes [addJumpCommand m e, addMoveCommand m e] )

stepInputSystem :: SystemFn
stepInputSystem events = do
  A.cmap maintainInputs
  inputM  <- get global :: System' PlayerInput
  events' <- emap $ stepPlayerCommands inputM
  -- clear away "justModified" key map
  set global (inputM { justModified = Map.empty })
  return $ events ++ events'
