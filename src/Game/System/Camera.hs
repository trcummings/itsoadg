module Game.System.Camera (cameraEvents) where

import qualified Linear as L
import qualified SDL
import           Apecs    (Not)
import           Data.Map ((!))
import           KeyState (isPressed, isTouched)

import           Game.Util.Constants (frameDeltaSeconds)
import           Game.Util.Move      (runMoveCommand, Moveable)
import           Game.Types
  ( PlayerInput(..)
  , Inputs(..)
  , HasMoveCommand(..)
  , MoveCommand(..)
  , Degrees(..)
  , Rotation(..)
  , Translation(..)
  , Camera(..)
  )

cameraEvents :: Inputs
             -> (Camera, Moveable, Not HasMoveCommand)
             -> Either Camera (Camera, HasMoveCommand)
cameraEvents inputs (p, _, _) =
  let m            = _inputs . _keyboardInput $ inputs
      leftPress    = isTouched $ m ! SDL.KeycodeA
      rightPress   = isTouched $ m ! SDL.KeycodeD
      forwardPress = isTouched $ m ! SDL.KeycodeW
      backPress    = isTouched $ m ! SDL.KeycodeS
      t            = realToFrac frameDeltaSeconds :: Float
      toDolly v    = Move'Translate (Translation $ v L.^* t)
      toRotat r    = Move'Rotate    Yaw (Degrees $ r * t)
      rt = if leftPress && rightPress
           then Nothing
           else if leftPress
                then Just $ toRotat 90
                else if rightPress
                     then Just $ toRotat (-90)
                     else Nothing
      vx = if forwardPress && backPress
           then Nothing
           else if backPress
                then Just $ toDolly $ L.V3 0 0 1
                else if forwardPress
                     then Just $ toDolly $ L.V3 0 0 (-1)
                     else Nothing
  in case (vx, rt) of
      (Nothing , Nothing ) -> Left p
      -- if both, bias rotation first
      (Just vx', Just rt') -> Right (p, HasMoveCommand (Move'Compose rt' vx'))
      (Just vx', _       ) -> Right (p, HasMoveCommand vx')
      (_       , Just rt') -> Right (p, HasMoveCommand rt')

-- import Linear (V2(..), (*^))
--
-- import Game.Wrapper.Apecs (Apecs(..))
-- import Game.Types
--   ( Camera(..), size, ppos
--   , CameraTarget(..)
--   , Acceleration(..)
--   , Position(..)
--   , QueueEvent(..) )
--
-- stepCameraPhysics :: Apecs m => [QueueEvent] -> m [QueueEvent]
-- stepCameraPhysics events = do
--   -- update camera position based on target
--   cmapM_ $ \(
--       Camera s@(V2 cw ch) cp
--     , CameraTarget e
--     , Acceleration a
--     , Position cpos
--     , camera ) -> do
--
--     (Position targetP) <- get e
--         -- target x y based on camera size
--     let txy = targetP - V2 (0.5 * cw) (0.5 * ch)
--         -- camera acceleration towards target
--         a'   = a + (txy - cpos)
--         -- ppos with drag
--         ppos' = cpos + (0.5 *^ (cp - cpos))
--         -- verlet on cpos
--         cpos' = cpos + (0.256 *^ a')
--         -- differentiate to get new cpos
--         d     = (2 *^ cpos') - ppos'
--     -- set new values
--     set camera (
--         Camera { size = s, ppos = cpos' }
--       , Acceleration $ V2 0 0
--       , Position d )
--   return events
