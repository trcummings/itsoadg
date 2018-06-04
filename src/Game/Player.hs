module Game.Player where

import qualified SDL
import qualified KeyState (isTouched, ksCounter, isHeld)
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
  , Gravity(..)
  , Seconds(..)
  , Player(..), PlayerAction(..), PlayerKey(..)
  , Animations(..)
  , SpriteSheet(..)
  , Step(..)
  , FlowEffectEmitter(..)
  , FlowEffectEmitState(..) )
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
import           Game.Step (smash, peel)
import           Game.World (System')

-- movement
bumpVelocityX :: Velocity -> Unit -> Velocity
bumpVelocityX (Velocity (V2 vx vy)) ax =
   Velocity $ V2 (vx + (ax * Unit frameDeltaSeconds)) vy

playerBothButtons :: Unit -> System' ()
playerBothButtons stopAccel = cmap $ \(Player _, v@(Velocity (V2 vx _))) ->
  bumpVelocityX v (
    if vx > 0
    then   stopAccel
    else (-stopAccel) )

playerRun :: Unit -> Unit -> Unit -> System' ()
playerRun stopAccel runAccel sign =
  cmapM_ $ \(Player _, v@(Velocity (V2 vx _)), e) -> do
    when (abs vx < playerTopSpeed) $ do
      let ax
           | sign ==   1  = if (vx < 0) then 3 * (-stopAccel) else   runAccel
           | sign == (-1) = if (vx > 0) then 3 *   stopAccel  else (-runAccel)
           | otherwise = 0
      set e (bumpVelocityX v ax)

toZeroVelocity :: Unit -> Unit -> Bool
toZeroVelocity stopAccel vx =
  if (vx > 0)
  then vx + nextVx <= 0
  else vx - nextVx >= 0
  where nextVx = stopAccel * Unit frameDeltaSeconds

playerStop :: Unit -> System' ()
playerStop stopAccel = cmap $ \(Player _, v@(Velocity (V2 vx vy))) ->
  let ax
       | willStopNext =   0
       | vx >  0      =   stopAccel
       | vx <  0      = (-stopAccel)
       | otherwise    =   0
       where willStopNext = toZeroVelocity stopAccel vx
  in if ax == 0
     then Velocity $ V2 0 vy
     else bumpVelocityX v ax

setJump :: System' ()
setJump = cmapM_ $ \(Player _, jumpState@(Jump _ _ _), e) -> do
  when (jumpState == onGround) $ set e jumpRequested

releaseJump :: System' ()
releaseJump = cmapM_ $ \(Player _, jumpState@(Jump _ _ _), e) -> do
  when (jumpState == landed) $ set e onGround

bumpSpeed :: Bool -> Bool -> Unit -> Unit -> System' ()
bumpSpeed True  False stopA runA = playerRun stopA runA (-1)
bumpSpeed False True  stopA runA = playerRun stopA runA  1
bumpSpeed False False stopA _    = playerStop stopA
bumpSpeed True  True  stopA _    = playerBothButtons stopA


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

stepPlayerState :: System' ()
stepPlayerState = do
  PlayerInput m <- get global
  let aPress = m ! SDL.KeycodeA
      dPress = m ! SDL.KeycodeD
      wPress = m ! SDL.KeycodeW
      nPress = m ! SDL.KeycodeN
      mPress = m ! SDL.KeycodeM

  cmapM_ $ \a@( Player _
            , jump@(Jump _ _ _)
            , g@(Gravity _ _)
            , Velocity (V2 vx _)
            , FlowEffectEmitter flowState
            , e ) -> do
    let isJumping  = jump == falling || jump == floating || jump == jumping
        runBumpSpeed = bumpSpeed (KeyState.isTouched aPress) (KeyState.isTouched dPress)

    -- attempt to jump
    case (KeyState.isTouched wPress) of
      True  ->
        case flowState of
          -- cannot jump while burning
          -- so play some kind of feedback, audio or whatever
          BurningFlow -> return ()
          _           -> setJump
      False -> releaseJump

    -- attempt to activate burning state
    when (KeyState.isHeld nPress) $ do
      case (KeyState.ksCounter nPress) of
        Just nCount ->
          if (nCount > 0.5)
          then case flowState of
            AbsorbingFlow -> return ()
            _             -> set e ( FlowEffectEmitter BurningFlow
                                   , Gravity { ascent  = initialJumpG
                                             , descent = initialJumpG / Unit 2 } )
          -- play some kind of sound here to indicate its "charging up"
          else return ()
        Nothing     -> return ()

    -- release burning state
    case (KeyState.isTouched nPress) of
      True  -> return ()
      False -> case flowState of
        BurningFlow ->
          set e ( FlowEffectEmitter NotEmittingFlowEffect
                , Gravity { ascent  = initialJumpG
                          , descent = initialFallG } )
        _           -> return ()

    -- attempt to activate absorbing state
    case (KeyState.isTouched mPress) of
      True  ->
        case flowState of
          BurningFlow -> return ()
          _           ->
            -- cannot absorb while jumping
            if isJumping
            then return ()
            else set e ( FlowEffectEmitter AbsorbingFlow
                       , Gravity { ascent  = initialJumpG / Unit 2
                                 , descent = initialFallG } )
      False -> return ()

    -- release absorbing state
    case (KeyState.isTouched mPress) of
      True  -> return ()
      False -> case flowState of
        AbsorbingFlow -> set e ( FlowEffectEmitter NotEmittingFlowEffect
                               , Gravity { ascent  = initialJumpG
                                         , descent = initialFallG } )
        _             -> return ()

    -- ensure gravity is set for proper burning state
    case flowState of
      NotEmittingFlowEffect -> set e (Gravity { ascent  = initialJumpG
                                              , descent = initialFallG })
      _                     -> return ()

    -- modify player speed
    case flowState of
      BurningFlow           ->
        if isJumping
        then do runBumpSpeed stoppingAccel  runningAccel
        else runBumpSpeed bStoppingAccel bRunningAccel
      AbsorbingFlow         -> runBumpSpeed aStoppingAccel aRunningAccel
      NotEmittingFlowEffect -> runBumpSpeed stoppingAccel  runningAccel


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

