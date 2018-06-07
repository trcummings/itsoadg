{-# LANGUAGE FlexibleContexts #-}

module Game.Physics where

import qualified SDL
import           Control.Monad (when, unless)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Map ((!))
import           Data.Coerce (coerce)
import           Data.List (partition)
import           Linear (V2(..), V4(..), (^*), (*^), (^/))
import qualified KeyState (isTouched)
import           Apecs
  ( Entity
  , cmap
  , cmapM_
  , set
  , get
  , getAll
  , proxy
  , exists
  , global
  , modify )

import           Game.World (System')
import           Game.AABB
  ( aabbCheck
  , sweepAABB
  , penetrationVector
  , broadPhaseAABB
  , inNarrowPhase )
import           Game.Collision
  ( toVector
  , resolveBaseCollision
  , resolveNormalVelocity
  , inverseNormal )
import           Game.Constants
  ( dT
  , frameDeltaSeconds
  , maxSpeed
  , playerTopSpeed
  , screenWidth
  , screenHeight
  , runningAccel
  , stoppingAccel
  , initialJumpVy
  , initialJumpG )
import           Game.Types
  ( Unit(..)
  , Jump(..), buttonPressed, isJumping, isGrounded
  , Acceleration(..)
  , Position(..)
  , Velocity(..)
  , Friction(..)
  , BoundingBox(..)
  , Gravity(..)
  , PlayerInput(..)
  , AABB(..), center, dims
  , BoxEntity(..)
  , Collision(..)
  , CollisionNormal(..)
  , PenetrationVector(..)
  , CollisionTime(..)
  , Inbox(..) )
import Game.Jump
  ( landed
  , onGround
  , jumpRequested
  , jumping
  , floating
  , falling )


oneBumpPerSecond :: Unit
oneBumpPerSecond = (4 / 32) * Unit frameDeltaSeconds


hasVelComponent :: BoxEntity -> System' Bool
hasVelComponent (_, _, e) = do
  hasVelocity <- exists e (proxy :: Velocity)
  return hasVelocity

handleBaseCollision :: Entity -> Collision -> System' ()
handleBaseCollision e c@(Collision _ _ _ _) = do
  pv <- get e :: System' (Position, Velocity)
  set e $ resolveBaseCollision c pv

handleJumpCheck :: Entity -> Collision -> System' ()
handleJumpCheck e (Collision _ normal _ _) = do
  hasJump <- exists e (proxy :: Jump)
  when (hasJump && normal == BottomNormal) $ do
    jumpState <- get e :: System' Jump
    when (jumpState == jumping) $ set e landed
    when (jumpState == floating || jumpState == falling) $ set e onGround


handlePostCollision :: Entity -> Collision -> System' ()
handlePostCollision e c = do
  -- handleFloor e c
  handleJumpCheck e c

type GetVelocity  = System' (Velocity)
type GetAABB      = System' (BoundingBox, Position)
type GetSweptAABB = System' (BoundingBox, Position, Velocity)


testCollision :: Entity -> BoxEntity -> System' ()
testCollision e (_, _, e') = do
  -- get current collision information
  (BoundingBox bb1, Position p1, v@(Velocity v')) <- get e :: GetSweptAABB
  (BoundingBox bb2, Position p2) <- get e' :: GetAABB
  let box1 = AABB { center = p1, dims = bb1 }
      box2 = AABB { center = p2, dims = bb2 }
      (collisionTime, normal) = sweepAABB v box1 box2
      (pVec, pNormal) = penetrationVector box1 box2
      pVector = PenetrationVector $ pVec * (toVector pNormal)
      collision = Collision collisionTime normal pVector e'
      lowCollisionTime = ((coerce collisionTime :: Double) * frameDeltaSeconds) < 0.00005
      noPenetration = (abs <$> (coerce pVector :: V2 Unit)) == V2 0 0
      hasZeroNormal = normal == NoneNormal
      useSimpleResolution =
            hasZeroNormal
        ||  lowCollisionTime
        || (hasZeroNormal && noPenetration)

  if (useSimpleResolution)
  -- collision too slow to use swept resolution, or we hit a corner
  then do
    let Velocity v'' = resolveNormalVelocity v pVector normal
        willNotEscape = aabbCheck
          (broadPhaseAABB (BoundingBox bb1) (Position p1) (Velocity v''))
          box2
        -- handle floor corners
        p' = if (hasZeroNormal && not noPenetration && willNotEscape)
             -- push out of wall
             then p1 + (coerce pVector :: V2 Unit)
             -- otherwise update normally
             else p1 + (v'' ^* Unit frameDeltaSeconds)
    set e (Velocity v'', Position p')
    -- dispatchToInbox
    --   (Collision collisionTime pNormal pVector e') e
    -- let reverseVector = PenetrationVector $ negate <$> (coerce pVector :: V2 Unit)
    --     reverseNormal = inverseNormal pNormal
    -- dispatchToInbox
    --   (Collision collisionTime reverseNormal reverseVector e) e'
  -- we need a swept collision resolution
  else handleBaseCollision e collision
  -- move on to continued resolution
  handlePostCollision e collision

stepPosition :: (Velocity, Position) -> (Velocity, Position)
stepPosition (v@(Velocity v'), Position p) =
  (v, Position $ p + (v' ^* Unit frameDeltaSeconds))

handleNotOnGround :: Entity -> System' ()
handleNotOnGround entity = do
  -- we are not on the ground if we arent colliding with anything, but we arent
  -- necessarily jumping
  hasJump <- exists entity (proxy :: Jump)
  when hasJump $ do
    jumpState <- get entity :: System' Jump
    when (jumpState == onGround || jumpState == landed) $ set entity falling

handleCollisions :: System' ()
handleCollisions = do
  -- get all entities with a bounding box
  allBoundingBoxes <- getAll :: System' [BoxEntity]

  -- partition into moving boxes & static boxes
  (dynamics, _) <- partitionM hasVelComponent allBoundingBoxes

  mapM_ (\(bb@(BoundingBox bb'), p@(Position p'), entity) -> do
       v@(Velocity v') <- get entity :: System' Velocity
       let sweptBox = broadPhaseAABB bb p v
           actives  = filter (inNarrowPhase entity sweptBox) allBoundingBoxes

       -- run all collision updates
       mapM_ (testCollision entity) actives

       -- when no collisions happened, update position and velocity normally
       when (length actives == 0) $ do
         handleNotOnGround entity
         -- update position based on time and velocity
         let p'' = p' + (v' ^* Unit frameDeltaSeconds)
         set entity (Position p'')
     ) dynamics

handleGravity :: (Gravity, Velocity) -> Velocity
handleGravity (g@(Gravity _ _), Velocity (V2 vx vy)) =
  if vy > 0
  then Velocity $ V2 vx (vy + ((descent g) * Unit frameDeltaSeconds))
  else Velocity $ V2 vx (vy + ((ascent  g) * Unit frameDeltaSeconds))

clampVelocity :: Unit -> Unit
clampVelocity v =
  if (v > 0)
  then min v maxSpeed
  else max v (-maxSpeed)

handleVelocityClamp :: Velocity -> Velocity
handleVelocityClamp (Velocity v@(V2 vx vy)) =
  if (vx > (-oneBumpPerSecond) && vx < oneBumpPerSecond)
  then Velocity $ V2 0 vy
  else Velocity $ clampVelocity <$> v

handleJumpRequest :: (Jump, Velocity) -> (Jump, Velocity)
handleJumpRequest (jumpState@(Jump _ _ _), v@(Velocity (V2 vx _))) =
  if (jumpState == jumpRequested)
  then (jumping, Velocity $ V2 vx (-initialJumpVy))
  else (jumpState, v)

stepPhysics :: System' ()
stepPhysics = do
  -- update acceleration based on gravity
  cmap handleGravity

  -- jump!
  cmap handleJumpRequest

  -- clamp velocity
  cmap handleVelocityClamp

  -- collisions
  -- position will only be modified in here (as well as other things)
  handleCollisions
