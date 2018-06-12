module Game.Collision where

import           Linear (V2(..), (^*), (^/))
import           Apecs (Entity, set, proxy, getAll, get, exists)
import           Data.Coerce (coerce)
import           Control.Monad (when)
import           Control.Monad.Extra (partitionM)


import Game.World (System')
import Game.Types
  ( Unit(..)
  , Position(..)
  , Velocity(..)
  , CollisionNormal(..)
  , CollisionTime(..)
  , PenetrationVector(..)
  , Collision(..)
  , CollisionModule(..)
  , BoxEntity(..)
  , BoundingBox(..)
  , AABB(..), dims, center
  , Jump(..) )
import Game.Jump
import Game.Constants (frameDeltaSeconds)
import           Game.Util.AABB
  ( aabbCheck
  , sweepAABB
  , penetrationVector
  , broadPhaseAABB
  , inNarrowPhase )
import           Game.Util.Collision
  ( toVector
  , resolveBaseCollision
  , resolveNormalVelocity
  , inverseNormal )

type GetVelocity  = System' (Velocity)
type GetAABB      = System' (BoundingBox, Position)
type GetSweptAABB = System' (BoundingBox, Position, Velocity)

hasVelComponent :: BoxEntity -> System' Bool
hasVelComponent (_, _, _, e) = do
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

handleNotOnGround :: Entity -> System' ()
handleNotOnGround entity = do
  -- we are not on the ground if we arent colliding with anything, but we arent
  -- necessarily jumping
  hasJump <- exists entity (proxy :: Jump)
  when hasJump $ do
    jumpState <- get entity :: System' Jump
    when (jumpState == onGround || jumpState == landed) $ set entity falling


testCollision :: Entity -> BoxEntity -> System' ()
testCollision e (_, _, _, e') = do
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


handleCollisions :: System' ()
handleCollisions = do
  -- get all entities with a bounding box
  allBoundingBoxes <- getAll :: System' [BoxEntity]

  -- partition into moving boxes & static boxes
  (dynamics, _) <- partitionM hasVelComponent allBoundingBoxes

  mapM_ (\(CollisionModule, bb@(BoundingBox bb'), p@(Position p'), entity) -> do
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
