{-# LANGUAGE FlexibleContexts #-}

module Game.Physics where

import qualified SDL
import           Control.Monad (when, unless)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Map ((!))
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
  , global )

import           Game.World (System')
import           Game.AABB
  ( AABB(..), center, dims
  , BoxEntity(..)
  , aabbCheck
  , sweepAABB
  , penetrationVector
  , broadPhaseAABB
  , inNarrowPhase )
import           Game.Collision
  ( Collision(..)
  , CNormal(..)
  , toVector
  , resolveBaseCollision
  , resolveNormalVelocity )
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
import           Game.Player (stepPlayerInput, stepPlayerAction)
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
  , PhysicsTime(..), time, accum
  , GlobalTime(..) )
import Game.Camera (stepCamera)
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
handleBaseCollision e c@(Collision _ _ _) = do
  pv <- get e :: System' (Position, Velocity)
  set e $ resolveBaseCollision c pv

-- handleFloor :: Entity -> Collision -> System' ()
-- handleFloor e c@(Collision _ normal e') = do
--   hasFriction <- exists e' (proxy :: Friction)
--   when hasFriction $ do
--     (Velocity (V2 vx vy)) <- get e  :: System' (Velocity)
--     (Friction f)          <- get e' :: System' (Friction)
--     case normal of
--       TopN    -> do
--         set e (Velocity $ V2 (vx * Unit f) vy)
--       BottomN -> do
--         set e (Velocity $ V2 (vx * Unit f) vy)
--       LeftN   -> do
--         set e (Velocity $ V2 vx (vy * Unit f))
--       RightN  -> do
--         set e (Velocity $ V2 vx (vy * Unit f))
--       _ -> return ()


handleJumpCheck :: Entity -> Collision -> System' ()
handleJumpCheck e (Collision _ normal _) = do
  hasJump <- exists e (proxy :: Jump)
  when (hasJump && normal == BottomN) $ do
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
      collision = Collision collisionTime normal e'
      pVector = penetrationVector box1 box2
      lowCollisionTime = collisionTime * frameDeltaSeconds < 0.00005
      noPenetration = (abs <$> pVector) == V2 0 0
      hasZeroNormal = normal == NoneN
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
             then p1 + pVector
             -- otherwise update normally
             else p1 + (v'' ^* Unit frameDeltaSeconds)
    set e (Velocity v'', Position p')
  -- we need a swept collision resolution
  else handleBaseCollision e collision
  -- move on to continued resolution
  handlePostCollision e collision


handleCollisions :: System' ()
handleCollisions = do
  -- get all entities with a bounding box
  allBoundingBoxes <- getAll :: System' [BoxEntity]

  -- partition into moving boxes & static boxes
  (dynamics, _) <- partitionM hasVelComponent allBoundingBoxes

  mapM_ (\(bb@(BoundingBox bb'), p@(Position p'), entity) -> do
       v@(Velocity v') <- get entity :: System' Velocity
       let sweptBox   = broadPhaseAABB bb p v
           actives    = filter (inNarrowPhase entity sweptBox) allBoundingBoxes

       -- run all collision updates
       mapM_ (testCollision entity) actives

       -- when no collisions happened, update position and velocity normally
       when (length actives == 0) $ do

         -- we are not on the ground if we arent colliding with anything, but we arent
         -- necessarily jumping
         hasJump <- exists entity (proxy :: Jump)
         when hasJump $ do
           jumpState <- get entity :: System' Jump

           when (jumpState == onGround || jumpState == landed) $ set entity falling

         -- update position based on time and velocity
         let p'' = p' + (v' ^* Unit frameDeltaSeconds)
         set entity (Position p'')
     ) dynamics


clampVelocity :: Unit -> Unit
clampVelocity v =
  if (v > 0)
  then min v maxSpeed
  else max v (-maxSpeed)

runPhysics :: System' ()
runPhysics = do
  -- run updates based on input map
  stepPlayerInput

  -- update acceleration based on gravity
  cmap $ \(Gravity, Velocity (V2 vx vy)) ->
    if vy > 0
    then Velocity $ V2 vx (vy + (3 * initialJumpG * Unit frameDeltaSeconds))
    else Velocity $ V2 vx (vy + (initialJumpG * Unit frameDeltaSeconds))

  -- jump!
  cmapM_ $ \(jumpState@(Jump _ _ _), e) -> do
    Velocity (V2 vx _) <- get e :: System' (Velocity)
    when (jumpState == jumpRequested) $ do
      set e (Velocity $ V2 vx (-initialJumpVy), jumping)

  -- clamp velocity
  cmap $ \(Velocity v@(V2 vx vy)) ->
    if (vx > (-oneBumpPerSecond) && vx < oneBumpPerSecond)
    then Velocity $ V2 0 vy
    else Velocity $ clampVelocity <$> v

   -- collisions
  -- position will only be modified in here (as well as other things)
  handleCollisions

  -- update player "action"
  stepPlayerAction

  -- update camera
  stepCamera

updatePhysicsAccum :: Double -> System' ()
updatePhysicsAccum nextTime = do
  GlobalTime currentTime <- get global
  -- update global time
  cmap $ \(GlobalTime _) -> GlobalTime nextTime

  -- update physics frame time accumulator
  cmap $ \(PhysicsTime t acc) -> PhysicsTime
    { time = t
    -- clamp frameTime at 25ms
    , accum = acc + (min 25 $ nextTime - currentTime) }

-- update physics multiple times if time step is less than frame update time
runPhysicsLoop :: System' ()
runPhysicsLoop = do
  PhysicsTime t acc <- get global
  when (acc >= dT) $ do
    runPhysics
    cmap $ \(PhysicsTime t' acc') -> PhysicsTime
      { time = (t' + dT)
      , accum = (acc' - dT) }
    runPhysicsLoop
