module Game.Player where

import qualified SDL
import qualified KeyState (isTouched)
import qualified Animate
import           Data.Map ((!))
import           Control.Monad (when)
import           Apecs (cmap, cmapM_, get, global, proxy, set)
import           Linear (V2(..))

import           Game.Types
  ( Velocity(..)
  , PlayerInput(..)
  , Jump(..)
  , Unit(..)
  , Seconds(..)
  , Player(..), PlayerAction(..), PlayerKey(..)
  , Animations(..) )
import           Game.Constants
  ( stoppingAccel
  , runningAccel
  , frameDeltaSeconds
  , playerTopSpeed )
import           Game.Jump (jumpRequested, onGround, landed)
import           Game.Step (Step(..))
import           Game.World (System')


stepPlayerAnimation :: Step PlayerAction
                    -> Animations PlayerKey
                    -> Animate.Position PlayerKey Seconds
                    -> Animate.Position PlayerKey Seconds
stepPlayerAnimation (Step'Sustain _) animations pos =
  Animate.stepPosition animations pos $ Seconds (realToFrac frameDeltaSeconds :: Float)
stepPlayerAnimation (Step'Change _ pa) _ _ = case pa of
  PlayerAction'MoveRight -> Animate.initPositionLoops PlayerKey'RWalk 0
  PlayerAction'JumpRight -> Animate.initPosition PlayerKey'RJump
  PlayerAction'IdleRight -> Animate.initPosition PlayerKey'RIdle
  PlayerAction'MoveLeft  -> Animate.initPositionLoops PlayerKey'LWalk 0
  PlayerAction'JumpLeft  -> Animate.initPosition PlayerKey'LJump
  PlayerAction'IdleLeft  -> Animate.initPosition PlayerKey'LIdle

-- stepPlayerState

-- movement
bumpVelocityX :: Velocity -> Unit -> Velocity
bumpVelocityX (Velocity (V2 vx vy)) ax =
   Velocity $ V2 (vx + (ax * Unit frameDeltaSeconds)) vy

playerBothButtons :: System' ()
playerBothButtons = cmap $ \(Player _, v@(Velocity (V2 vx _))) ->
  bumpVelocityX v (
    if vx > 0
    then   stoppingAccel
    else (-stoppingAccel) )

playerRun :: Unit -> System' ()
playerRun sign = cmapM_ $ \(Player _, v@(Velocity (V2 vx _)), e) -> do
  when (abs vx < playerTopSpeed) $ do
    let ax
         | sign ==   1  = if (vx < 0) then 2 * (-stoppingAccel) else   runningAccel
         | sign == (-1) = if (vx > 0) then 2 *   stoppingAccel  else (-runningAccel)
         | otherwise = 0
    set e (bumpVelocityX v ax)

playerStop :: System' ()
playerStop = cmap $ \(Player _, v@(Velocity (V2 vx vy))) ->
  let ax
       | vx > 0    =   stoppingAccel
       | vx < 0    = (-stoppingAccel)
       | otherwise = 0
  in  bumpVelocityX v ax

setJump :: System' ()
setJump = cmapM_ $ \(Player _, jumpState@(Jump _ _ _), e) -> do
  when (jumpState == onGround) $ set e jumpRequested

releaseJump :: System' ()
releaseJump = cmapM_ $ \(Player _, jumpState@(Jump _ _ _), e) -> do
  when (jumpState == landed) $ set e onGround

bumpSpeed :: Bool -> Bool -> System' ()
bumpSpeed True  False = playerRun (-1)
bumpSpeed False True  = playerRun   1
bumpSpeed False False = playerStop
bumpSpeed True  True  = playerBothButtons

stepPlayerInput :: System' ()
stepPlayerInput = do
  PlayerInput m <- get global
  let aPress = m ! SDL.KeycodeA
      dPress = m ! SDL.KeycodeD
      wPress = m ! SDL.KeycodeW

  bumpSpeed (KeyState.isTouched aPress) (KeyState.isTouched dPress)

  case (KeyState.isTouched wPress) of
    True  -> setJump
    False -> releaseJump

