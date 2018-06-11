{-# LANGUAGE FlexibleContexts #-}

module Game.Player where

import qualified SDL
import qualified KeyState (isTouched, ksCounter, isHeld)
import qualified Animate
import           Control.Lens hiding (get, set)
import           Data.Map ((!))
import           Control.Monad (when, foldM)
import           Control.Monad.IO.Class (liftIO)
import           Apecs (Entity, cmap, cmapM, cmapM_, get, getAll, global, proxy, set)
import           Linear (V2(..))

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
import           Game.World (System')

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
  if (not $ abs vx < playerTopSpeed)
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
  , FlowEffectEmitter
  , Entity )

type StepPlayer = (PlayerStateGroup, [QueueEvent])

setPlayerJumpSFX :: StepPlayer -> StepPlayer
setPlayerJumpSFX p =
  let entity = p ^._1._6
      jump   = p ^._1._2
      event  = AudioSystemEvent (entity, Player'SFX'Jump, Audio'PlayOrSustain)
  in if (not $ isPlayerJumping $ jump)
     then p & _2 %~ ((++) [event])
     else p

stepPlayerJump :: PlayerInputMap -> StepPlayer -> StepPlayer
stepPlayerJump m psg =
  if (KeyState.isTouched $ m ! SDL.KeycodeW)
  then case (flowState psg) of
    -- cannot jump while burning
    -- so play some kind of feedback, audio or whatever
         FlowEffectEmitter BurningFlow -> psg
         _                             -> (setPlayerJump . setPlayerJumpSFX) psg
   else releasePlayerJump psg
   where flowState         p = p ^._1._5
         releasePlayerJump p = p & _1._2 %~ releaseJump
         setPlayerJump     p = p & _1._2 %~ setJump


stepStartBurnState :: PlayerInputMap -> StepPlayer -> StepPlayer
stepStartBurnState m psg =
  -- attempt to activate burning state
  let nPress    = m ! SDL.KeycodeN
      flowState = psg ^._1._5
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
               psg & _1._5 .~ FlowEffectEmitter BurningFlow
                   & _1._3 .~ Gravity { ascent  = initialJumpG
                                      , descent = initialJumpG / Unit 2 }


stepReleaseBurnState :: PlayerInputMap -> StepPlayer -> StepPlayer
stepReleaseBurnState m psg =
  -- release burning state
  let nPress                        = m ! SDL.KeycodeN
      (FlowEffectEmitter flowState) = psg ^._1._5
  in if (KeyState.isTouched nPress)
     then psg
     else
       case flowState of
         BurningFlow ->
           psg & _1._5 .~ FlowEffectEmitter BurningFlow
               & _1._3 .~ Gravity { ascent  = initialJumpG
                                  , descent = initialFallG }
         _ -> psg


stepStartAbsorbState :: PlayerInputMap -> StepPlayer -> StepPlayer
stepStartAbsorbState m psg =
  -- attempt to activate absorbing state
  let mPress                        = m ! SDL.KeycodeM
      (FlowEffectEmitter flowState) = psg ^._1._5
      jumpState                     = psg ^._1._2
  in if (not $ KeyState.isTouched mPress)
     then psg
     else
       case flowState of
         BurningFlow -> psg
         _           ->
           if (isPlayerJumping jumpState)
           then psg
           else psg & _1._5 .~ FlowEffectEmitter AbsorbingFlow
                    & _1._3 .~ Gravity { ascent  = initialJumpG / Unit 2
                                       , descent = initialFallG }


stepReleaseAbsorbState :: PlayerInputMap -> StepPlayer -> StepPlayer
stepReleaseAbsorbState m psg =
  -- release absorbing state
  let mPress    = m ! SDL.KeycodeM
      (FlowEffectEmitter flowState) = psg ^._1._5
  in if (KeyState.isTouched mPress)
     then psg
     else
       case flowState of
         AbsorbingFlow ->
           psg & _1._5 .~ FlowEffectEmitter NotEmittingFlowEffect
               & _1._3 .~ Gravity { ascent  = initialJumpG
                                  , descent = initialFallG }
         _ -> psg


stepPlayerGravity :: PlayerInputMap -> StepPlayer -> StepPlayer
stepPlayerGravity m psg =
  -- ensure gravity is set for proper burning state
  let (FlowEffectEmitter flowState) = psg ^._1._5
  in case flowState of
      NotEmittingFlowEffect ->
        psg & _1._3 .~ Gravity { ascent  = initialJumpG
                               , descent = initialFallG }
      _ -> psg


stepPlayerSpeed :: PlayerInputMap -> StepPlayer -> StepPlayer
stepPlayerSpeed m psg =
  -- modify player speed
  let (FlowEffectEmitter flowState) = psg ^._1._5
      jumpState                     = psg ^._1._2
      velocity                      = psg ^._1._4
      aPress       = m ! SDL.KeycodeA
      dPress       = m ! SDL.KeycodeD
      runBumpSpeed =
        bumpSpeed velocity (KeyState.isTouched aPress) (KeyState.isTouched dPress)
  in psg & _1._4 .~ case flowState of
    BurningFlow           ->
        if isPlayerJumping jumpState
        then runBumpSpeed stoppingAccel  runningAccel
        else runBumpSpeed bStoppingAccel bRunningAccel
    AbsorbingFlow         -> runBumpSpeed aStoppingAccel aRunningAccel
    NotEmittingFlowEffect -> runBumpSpeed stoppingAccel  runningAccel


stepPlayerState :: System' [QueueEvent]
stepPlayerState = do
  PlayerInput m <- get global
  p:_ <- getAll :: System' [PlayerStateGroup]
  let (p', qs) = (
          (stepPlayerSpeed m)
          . (stepPlayerGravity m)
          . (stepReleaseAbsorbState m)
          . (stepStartAbsorbState m)
          . (stepReleaseBurnState m)
          . (stepStartBurnState m)
          . (stepPlayerJump m) ) (p, [])
      (a, b, c, d, e, entity) = p'
  set entity (a, b, c, d, e)
  return qs


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

