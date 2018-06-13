module Game.Collision where

import           Linear (V2(..), (^*), (^/))
import           Apecs (Entity, cmap, cmapM_, set, proxy, getAll, get, exists)
import           Data.Coerce (coerce)
import           Data.Map ((!?))
import qualified Data.Map as Map (Map, insertWith, empty)
import           Control.Monad (when)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)


import Game.World (System')
import Game.Types
  ( Unit(..)
  , Position(..)
  , Velocity(..)
  , CollisionNormal(..)
  , CollisionTime(..)
  , PenetrationVector(..)
  , CollisionModule(..)
  , BoxEntity(..)
  , BoundingBox(..)
  , AABB(..), dims, center
  , Jump(..)
  , Player(..)
  , HardFlow(..)
  , QueueEvent(..) )
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

type Collidable = (CollisionModule, BoundingBox, Position, Entity)
type DynamicCollidable = (CollisionModule, BoundingBox, Position, Velocity, Entity)

data To = To Entity deriving Show
data From = From Entity deriving Show
data CollisionType =
    NoCollision
  | SweptCollision  (CollisionTime    , CollisionNormal)
  | SimpleCollision (PenetrationVector, CollisionNormal)
  deriving Show

type CollisionMap = Map.Map Entity [CollisionType]
type Collision = (To, From, CollisionType)


determineCollisionType :: DynamicCollidable -> Collidable -> Collision
determineCollisionType (_, BoundingBox bb1, Position p1, v@(Velocity v'), e1)
                       (_, BoundingBox bb2, Position p2, e2) =
  let box1 = AABB { center = p1, dims = bb1 }
      box2 = AABB { center = p2, dims = bb2 }
      (collisionTime, normal) = sweepAABB v box1 box2
      (pVec, pNormal) = penetrationVector box1 box2
      pVector = PenetrationVector $ pVec * (toVector pNormal)
      lowCollisionTime = ((coerce collisionTime :: Double) * frameDeltaSeconds) < 0.00005
      noPenetration = (abs <$> (coerce pVector :: V2 Unit)) == V2 0 0
      hasZeroNormal = normal == NoneNormal
      toEvent = (,,) (To e1) (From e2)
      useSimpleResolution =
            hasZeroNormal
        ||  lowCollisionTime
        || (hasZeroNormal && noPenetration)
  -- in toEvent $ SweptCollision (collisionTime, normal)
  in if (useSimpleResolution)
     then
       let adjustedV = resolveNormalVelocity v pVector normal
           -- adjustedP = Position $ p1 + v' ^* Unit frameDeltaSeconds
           -- adjustedBBB = broadPhaseAABB (BoundingBox bb1) adjustedP adjustedV
           adjustedBBB = broadPhaseAABB (BoundingBox bb1) (Position p1) adjustedV
           willNotEscape = aabbCheck adjustedBBB box2
                 in if (hasZeroNormal && not noPenetration && willNotEscape)
          then toEvent $ SimpleCollision (pVector, pNormal)
          else toEvent NoCollision
     else toEvent $ SweptCollision (collisionTime, normal)


addToCollisionMap :: Collision -> CollisionMap -> CollisionMap
addToCollisionMap (To e1, From e2, collisionType) m =
  case collisionType of
    NoCollision                    -> m
    SimpleCollision (pVec, normal) ->
      let reverseVector = PenetrationVector $ negate <$> (coerce pVec :: V2 Unit)
          reverseNormal = inverseNormal normal
      in Map.insertWith (++) e2 [SimpleCollision (reverseVector, reverseNormal)] $
           Map.insertWith (++) e1 [collisionType] m
    SweptCollision (cTime, normal) ->
      let reverseNormal = inverseNormal normal
          neutralVector = PenetrationVector $ V2 0 0
      in Map.insertWith (++) e2 [SimpleCollision (neutralVector, reverseNormal)] $
           Map.insertWith (++) e1 [collisionType] m

processCollidable :: [Collidable] -> DynamicCollidable -> [Collision]
processCollidable allCollidables
                  cm@( _
                     , bb@(BoundingBox bb')
                     , p@(Position p')
                     , v@(Velocity v')
                     , entity ) =
  let sweptBox = broadPhaseAABB bb p v
      actives  = filter (inNarrowPhase entity sweptBox) allCollidables
      -- if we have no swept phase collisions
  in if (length actives == 0)
     then [(To entity, From entity, NoCollision)]
     else map (determineCollisionType cm) actives


-- post collection updates
-- util
getCollisions :: CollisionMap -> Entity -> [CollisionType]
getCollisions cm e = case (cm !? e) of Just c  -> c
                                       Nothing -> []

-- jump
stepJumpState :: CollisionType -> Jump -> Jump
stepJumpState collisionType jumpState =
  if (normal == BottomNormal)
  then if (jumpState == jumping)
  then landed
  else if (jumpState == floating || jumpState == falling)
       then onGround
       else jumpState
  else jumpState
  where normal = case collisionType of SimpleCollision (_, normal) -> normal
                                       SweptCollision  (_, normal) -> normal

stepCollisionJump :: CollisionMap -> (CollisionModule, Jump, Entity) -> Jump
stepCollisionJump cm (_, jumpState, e) =
  let collisions = getCollisions cm e
  in if (length collisions == 0)
     then if (jumpState == onGround || jumpState == landed)
          then falling
          else jumpState
     else foldr stepJumpState jumpState collisions


-- speed
stepSpeed :: CollisionType -> Velocity -> Velocity
stepSpeed (SimpleCollision (pVec, normal)) v = resolveNormalVelocity v pVec normal
stepSpeed (SweptCollision (cTime, normal)) v = resolveBaseCollision (cTime, normal) v

stepCollisionSpeed :: CollisionMap -> (CollisionModule, Velocity, Entity) -> Velocity
stepCollisionSpeed cm (_, v, e) =
  let collisions = getCollisions cm e
  in if (length collisions == 0)
     then v
     else foldr stepSpeed v collisions


-- position
stepPosition :: CollisionType -> (Position, Velocity) -> (Position, Velocity)
stepPosition (SimpleCollision (pVec, normal))
             (Position p, v@(Velocity v')) =
  (Position $ p + (coerce pVec :: V2 Unit), v)
stepPosition (SweptCollision (CollisionTime cTime, normal))
             (Position p, v@(Velocity v')) =
  let cTime' = (coerce cTime :: Double) * frameDeltaSeconds
      -- lowCollisionTime = cTime' < 0.00005
      -- time = if (lowCollisionTime) then frameDeltaSeconds else cTime'
  in (Position $ p + v' ^* Unit cTime', v)

stepCollisionPosition :: CollisionMap
                      -> (CollisionModule, Velocity, Position, Entity)
                      -> Position
stepCollisionPosition cm (_, v@(Velocity v'), p@(Position p'), e) =
  let collisions = getCollisions cm e
  in if (length collisions == 0)
     then Position $ p' + (v' ^* Unit frameDeltaSeconds)
     else fst $ foldr stepPosition (p, v) collisions


-- whole system
stepCollisionSystem :: System' ()
stepCollisionSystem = do
  -- get all entities with a collision module
  allCollidables    <- getAll :: System' [Collidable]
  movingCollidables <- getAll :: System' [DynamicCollidable]
  -- compute collisions, insert in entity-key collision-value map
  let collisions   = concat $ map (processCollidable allCollidables) movingCollidables
      collisionMap = foldr addToCollisionMap Map.empty collisions
  -- process CollisionModule related components
  -- cmapM_ $ \(Player _, j@(Jump _ _ _), p@(Position _), v@(Velocity _)) -> do
  --   liftIO $ putStrLn "__________________"
  --   liftIO $ putStrLn $ show j ++ ", " ++ show p ++ ", " ++ show v

  cmap $ (stepCollisionSpeed    collisionMap)
  cmap $ (stepCollisionPosition collisionMap)
  cmap $ (stepCollisionJump     collisionMap)

  -- liftIO $ putStrLn $ show collisions

  -- cmapM_ $ \(Player _, j@(Jump _ _ _), p@(Position _), v@(Velocity _)) -> do
  --   liftIO $ putStrLn $ show j ++ ", " ++ show p ++ ", " ++ show v
  --   liftIO $ putStrLn "__________________"
  -- cmap $ \(CollisionModule, HardFlow)       -> HardFlow
  -- cmap $ \(CollisionModule)                 -> CollisionModule
