module Game.Player where

import qualified SDL
import qualified KeyState (isTouched, ksCounter)
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
  , SpriteSheet(..)
  , Step(..) )
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
import           Game.Step (smash, peel)
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
         | sign ==   1  = if (vx < 0) then 3 * (-stoppingAccel) else   runningAccel
         | sign == (-1) = if (vx > 0) then 3 *   stoppingAccel  else (-runningAccel)
         | otherwise = 0
    set e (bumpVelocityX v ax)

toZeroVelocity :: Unit -> Bool
toZeroVelocity vx =
  if (vx > 0)
  then vx + nextVx <= 0
  else vx - nextVx >= 0
  where nextVx = stoppingAccel * Unit frameDeltaSeconds

playerStop :: System' ()
playerStop = cmap $ \(Player _, v@(Velocity (V2 vx vy))) ->
  let ax
       | willStopNext =   0
       | vx >  0      =   stoppingAccel
       | vx <  0      = (-stoppingAccel)
       | otherwise    =   0
       where willStopNext = toZeroVelocity vx
  in if ax == 0
     then Velocity $ V2 0 vy
     else bumpVelocityX v ax

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
      nPress = m ! SDL.KeycodeN

  bumpSpeed (KeyState.isTouched aPress) (KeyState.isTouched dPress)

  case (KeyState.isTouched wPress) of
    True  -> setJump
    False -> releaseJump

  case (KeyState.ksCounter nPress) of
    Just nCount ->
      when (nCount > 0.5) $ do
        liftIO $ putStrLn "burning flow!"
    Nothing     -> return ()


data Dir = L | R

actionDir :: PlayerAction -> Dir
actionDir action = case action of
  PlayerAction'MoveRight -> R
  PlayerAction'JumpRight -> R
  PlayerAction'IdleRight -> R
  PlayerAction'MoveLeft  -> L
  PlayerAction'JumpLeft  -> L
  PlayerAction'IdleLeft  -> L

moveAction :: Dir -> PlayerAction
moveAction L = PlayerAction'MoveLeft
moveAction R = PlayerAction'MoveRight

jumpAction :: Dir -> PlayerAction
jumpAction L = PlayerAction'JumpLeft
jumpAction R = PlayerAction'JumpRight

idleAction :: Dir -> PlayerAction
idleAction L = PlayerAction'IdleLeft
idleAction R = PlayerAction'IdleRight

stepPlayerAction :: System' ()
stepPlayerAction = do
  cmapM_ $ \( Player pastActionStep
            , jump@(Jump _ _ _)
            , Velocity (V2 vx _)
            , SpriteSheet sheet position
            , e ) -> do
    let pastAction = smash pastActionStep
        pastDir    = actionDir pastAction
        isJumping  = jump == falling || jump == floating || jump == jumping
        nextAction = if isJumping
                     then jumpAction pastDir
                     else if (vx == 0)
                          then idleAction pastDir
                          else if (vx > 0)
                               then moveAction R
                               else moveAction L
        nextActionStep = if (pastAction == nextAction)
                         then Step'Sustain nextAction
                         else Step'Change  pastAction nextAction
        animations = Animate.ssAnimations sheet :: Animations PlayerKey
        position'  = stepPlayerAnimation nextActionStep animations position
    set e (Player nextActionStep, SpriteSheet sheet position')
