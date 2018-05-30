module Game.Player where

import qualified SDL
import qualified KeyState (isTouched)
import qualified Animate
import           Data.Map ((!))
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Apecs (cmap, cmapM_, get, global, proxy, set)
import           Linear (V2(..))

import           Game.Types
  ( Velocity(..)
  , PlayerInput(..)
  , Jump(..)
  , Unit(..)
  , Seconds(..)
  , Player(..), PlayerAction(..), PlayerKey(..)
  , Animations(..)
  , SpriteSheet(..) )
import           Game.Constants
  ( stoppingAccel
  , runningAccel
  , frameDeltaSeconds
  , playerTopSpeed )
import           Game.Jump
  ( jumpRequested
  , onGround
  , landed
  , falling
  , floating
  , jumping )
import           Game.Step (Step(..), smash, peel)
import           Game.World (System')


stepPlayerAnimation :: Step PlayerAction
                    -> Animations PlayerKey
                    -> Animate.Position PlayerKey Seconds
                    -> Animate.Position PlayerKey Seconds
stepPlayerAnimation (Step'Sustain _) animations pos =
  Animate.stepPosition animations pos $ Seconds (realToFrac frameDeltaSeconds :: Float)
stepPlayerAnimation (Step'Change _ pa) _ _ = case pa of
  PlayerAction'MoveRight -> Animate.initPositionWithLoop PlayerKey'RWalk Animate.Loop'Always
  PlayerAction'JumpRight -> Animate.initPosition PlayerKey'RJump
  PlayerAction'IdleRight -> Animate.initPosition PlayerKey'RIdle
  PlayerAction'MoveLeft  -> Animate.initPositionWithLoop PlayerKey'LWalk Animate.Loop'Always
  PlayerAction'JumpLeft  -> Animate.initPosition PlayerKey'LJump
  PlayerAction'IdleLeft  -> Animate.initPosition PlayerKey'LIdle

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

stepPlayerAction :: System' ()
stepPlayerAction = do
  cmapM_ $ \(Player spa, jump@(Jump _ _ _), Velocity (V2 vx _), SpriteSheet ss ap, e) -> do
    let pa = smash spa
        na = if (jump == falling || jump == floating || jump == jumping)
             then PlayerAction'JumpRight
             else if (vx > 0 || vx < 0)
                  then PlayerAction'MoveRight
                  else PlayerAction'IdleRight
        sna = if (pa == na)
              then Step'Sustain na
              else Step'Change  pa na
        animations = Animate.ssAnimations ss :: Animations PlayerKey
        ap' = stepPlayerAnimation spa animations ap

    set e (Player sna, SpriteSheet ss ap')
