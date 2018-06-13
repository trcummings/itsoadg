{-# LANGUAGE FlexibleContexts #-}

module Game.Player where

import qualified SDL
import qualified KeyState
import qualified Animate
import           Control.Lens hiding (get, set)
import           Data.Map ((!))
import           Control.Monad (when, foldM)
import           Control.Monad.IO.Class (liftIO)
import           Apecs (Entity, cmap, cmapM, cmapM_, get, getAll, global, proxy, set)
import           Linear (V2(..))

import           Game.Wrapper.Apecs (emap)
import           Game.Types
  ( Velocity(..)
  , PlayerInput(..), PlayerInputMap
  , Jump(..)
  , Unit(..)
  , Gravity(..)
  , Seconds(..)
  , Player(..), PlayerAction(..), PlayerKey(..), Player'SFX'Key(..)
  , Animations(..)
  , SpriteSheet(..)
  , Step(..)
  , FlowEffectEmitter(..)
  , FlowEffectEmitState(..)
  , Audio'Command(..)
  , QueueEvent(..) )
import           Game.Constants
  ( initialJumpG
  , initialFallG
  , stoppingAccel
  , runningAccel
  , bRunningAccel
  , bStoppingAccel
  , aRunningAccel
  , aStoppingAccel
  , frameDeltaSeconds
  , playerTopSpeed )
import           Game.Jump
  ( jumpRequested
  , onGround
  , landed
  , falling
  , floating
  , jumping )
-- import           Game.Audio (dispatchToAudioInbox)
import           Game.Step (smash, peel)
import           Game.World (System', SystemFn)

-- movement
bumpVelocityX :: Velocity -> Unit -> Velocity
bumpVelocityX (Velocity (V2 vx vy)) ax =
   Velocity $ V2 (vx + (ax * Unit frameDeltaSeconds)) vy

playerBothButtons :: Velocity -> Unit -> Velocity
playerBothButtons v@(Velocity (V2 vx _)) stopAccel =
  bumpVelocityX v (
    if vx > 0
    then   stopAccel
    else (-stopAccel) )

playerRun :: Velocity -> Unit -> Unit -> Unit -> Velocity
playerRun v@(Velocity (V2 vx _)) stopAccel runAccel sign =
  if (not (abs vx < playerTopSpeed))
  then v
  else
    let ax
          | sign ==   1  = if (vx < 0) then 3 * (-stopAccel) else   runAccel
          | sign == (-1) = if (vx > 0) then 3 *   stopAccel  else (-runAccel)
          | otherwise = 0
    in bumpVelocityX v ax

toZeroVelocity :: Unit -> Unit -> Bool
toZeroVelocity stopAccel vx =
  if (vx > 0)
  then vx + nextVx <= 0
  else vx - nextVx >= 0
  where nextVx = stopAccel * Unit frameDeltaSeconds

playerStop :: Velocity -> Unit -> Velocity
playerStop v@(Velocity (V2 vx vy)) stopAccel =
  let ax
       | willStopNext =   0
       | vx >  0      =   stopAccel
       | vx <  0      = (-stopAccel)
       | otherwise    =   0
       where willStopNext = toZeroVelocity stopAccel vx
  in if ax == 0
     then Velocity $ V2 0 vy
     else bumpVelocityX v ax

bumpSpeed :: Velocity -> Bool -> Bool -> Unit -> Unit -> Velocity
bumpSpeed v True  False stopA runA = playerRun v stopA runA (-1)
bumpSpeed v False True  stopA runA = playerRun v stopA runA   1
bumpSpeed v False False stopA _    = playerStop v stopA
bumpSpeed v True  True  stopA _    = playerBothButtons v stopA

isPlayerJumping :: Jump -> Bool
isPlayerJumping j = j == falling || j == floating || j == jumping

setJump :: Jump -> Jump
setJump jumpState =
  if jumpState == onGround
  then jumpRequested
  else jumpState

releaseJump :: Jump -> Jump
releaseJump jumpState =
  if (jumpState == landed)
  then onGround
  else jumpState


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


type PlayerStateGroup =
  ( Player
  , Jump
  , Gravity
  , Velocity
  , FlowEffectEmitter )

-- lenses for PlayerStateGroup
_player :: Lens' PlayerStateGroup Player
_player = _1

_jump :: Lens' PlayerStateGroup Jump
_jump = _2

_gravity :: Lens' PlayerStateGroup Gravity
_gravity = _3

_velocity :: Lens' PlayerStateGroup Velocity
_velocity = _4

_flowState :: Lens' PlayerStateGroup FlowEffectEmitter
_flowState = _5

stepPlayerJump :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepPlayerJump m psg =
  if (KeyState.isTouched $ m ! SDL.KeycodeW)
  then case (psg ^._flowState) of
    -- cannot jump while burning
    -- so play some kind of feedback, audio or whatever
         FlowEffectEmitter BurningFlow -> psg
         _                             -> psg & _jump %~ setJump
  else psg & _jump %~ releaseJump


stepStartBurnState :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepStartBurnState m psg =
  -- attempt to activate burning state
  let nPress    = m ! SDL.KeycodeN
      flowState = psg ^._flowState
  in if (not $ KeyState.isHeld nPress)
     then psg
     else
       case (KeyState.ksCounter nPress) of
         Nothing     -> psg
         Just nCount ->
           if (nCount < 0.5)
           -- charging up
           then psg
           -- activated
           else case flowState of
             FlowEffectEmitter AbsorbingFlow -> psg
             _ ->
               psg & _flowState .~ FlowEffectEmitter BurningFlow
                   & _gravity   .~ Gravity { ascent  = initialJumpG
                                           , descent = initialJumpG / Unit 2 }


stepReleaseBurnState :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepReleaseBurnState m psg =
  -- release burning state
  let nPress                        = m ! SDL.KeycodeN
      (FlowEffectEmitter flowState) = psg ^._flowState
  in if (KeyState.isTouched nPress)
     then psg
     else
       case flowState of
         BurningFlow ->
           psg & _flowState .~ FlowEffectEmitter BurningFlow
               & _gravity   .~ Gravity { ascent  = initialJumpG
                                       , descent = initialFallG }
         _ -> psg


stepStartAbsorbState :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepStartAbsorbState m psg =
  -- attempt to activate absorbing state
  let mPress                        = m ! SDL.KeycodeM
      (FlowEffectEmitter flowState) = psg ^._flowState
      jumpState                     = psg ^._jump
  in if (not $ KeyState.isTouched mPress)
     then psg
     else
       case flowState of
         BurningFlow -> psg
         _           ->
           if (isPlayerJumping jumpState)
           then psg
           else psg & _flowState .~ FlowEffectEmitter AbsorbingFlow
                    & _gravity   .~ Gravity { ascent  = initialJumpG / Unit 2
                                            , descent = initialFallG }


stepReleaseAbsorbState :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepReleaseAbsorbState m psg =
  -- release absorbing state
  let mPress    = m ! SDL.KeycodeM
      (FlowEffectEmitter flowState) = psg ^._flowState
  in if (KeyState.isTouched mPress)
     then psg
     else
       case flowState of
         AbsorbingFlow ->
           psg & _flowState .~ FlowEffectEmitter NotEmittingFlowEffect
               & _gravity   .~ Gravity { ascent  = initialJumpG
                                       , descent = initialFallG }
         _ -> psg


stepPlayerGravity :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepPlayerGravity m psg =
  -- ensure gravity is set for proper burning state
  let (FlowEffectEmitter flowState) = psg ^._flowState
  in case flowState of
      NotEmittingFlowEffect ->
        psg & _gravity .~ Gravity { ascent  = initialJumpG
                                  , descent = initialFallG }
      _ -> psg


stepPlayerSpeed :: PlayerInputMap -> PlayerStateGroup -> PlayerStateGroup
stepPlayerSpeed m psg =
  -- modify player speed
  let (FlowEffectEmitter flowState) = psg ^._flowState
      jumpState                     = psg ^._jump
      velocity                      = psg ^._velocity
      aPress       = m ! SDL.KeycodeA
      dPress       = m ! SDL.KeycodeD
      runBumpSpeed =
        bumpSpeed velocity (KeyState.isTouched aPress) (KeyState.isTouched dPress)
  in psg & _velocity .~ case flowState of
    BurningFlow           ->
        if isPlayerJumping jumpState
        then runBumpSpeed stoppingAccel  runningAccel
        else runBumpSpeed bStoppingAccel bRunningAccel
    AbsorbingFlow         -> runBumpSpeed aStoppingAccel aRunningAccel
    NotEmittingFlowEffect -> runBumpSpeed stoppingAccel  runningAccel


stepPlayerState :: SystemFn
stepPlayerState evts = do
  PlayerInput m <- get global
  cmap $ (
      (stepPlayerSpeed m)
    . (stepPlayerGravity m)
    . (stepReleaseAbsorbState m)
    . (stepStartAbsorbState m)
    . (stepReleaseBurnState m)
    . (stepStartBurnState m)
    . (stepPlayerJump m) )
  return evts

stepPlayerAction :: System' ()
stepPlayerAction = do
  cmapM_ $ \( Player pastActionStep
            , jump@(Jump _ _ _)
            , Velocity (V2 vx _)
            , SpriteSheet sheet position
            , e ) -> do
    let pastAction = smash pastActionStep
        pastDir    = actionDir pastAction
        nextAction = if isPlayerJumping jump
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

