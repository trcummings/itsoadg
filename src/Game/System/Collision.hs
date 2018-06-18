module Game.System.Collision where

import           Linear (V2(..), (^*), (^/))
import           Apecs (Entity, cmap, cmapM_, set, proxy, getAll, get, exists)
import           Data.Coerce (coerce)
import           Data.Map ((!?))
import           Data.Maybe (catMaybes)
import qualified Data.Map as Map (Map, insertWith, empty)
import           Control.Monad (when)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)


import           Game.World (System')
import           Game.Types
  ( Unit(..)
  , Position(..)
  , Velocity(..)
  , CollisionNormal(..)
  , CollisionTime(..)
  , CollisionType(..)
  , PenetrationVector(..)
  , CollisionModule(..)
  , CollisionLayer(..)
  , BoxEntity(..)
  , BoundingBox(..)
  , AABB(..), dims, center
  , Jump(..)
  , Player(..)
  , HardFlow(..)
  , QueueEvent(..)
  , Audio'Command(..)
  , Player'SFX'Key(..)
  , To(..)
  , From(..)
  , TileMap(..)
  , TileType(..) )
import           Game.Wrapper.Apecs (emap)
import           Game.Util.Constants (frameDeltaSeconds, onePixel)
import           Game.Util.TileMap
  ( basicTilemap
  , getTileTypeAt
  , raycastAlongX
  , raycastAlongY )
import           Game.Util.AABB
  ( aabbCheck
  , sweepAABB
  , aabbMin
  , aabbMax
  , aabbBottomLeft
  , aabbTopRight
  , penetrationVector
  , broadPhaseAABB
  , inNarrowPhase )
import           Game.Util.Collision
  ( toVector
  , resolveBaseCollision
  , resolveNormalVelocity
  , inverseNormal
  , areLayersCollidable )

type GetVelocity  = System' (Velocity)
type GetAABB      = System' (BoundingBox, Position)
type GetSweptAABB = System' (BoundingBox, Position, Velocity)

type Collidable = (CollisionModule, BoundingBox, Position, Entity)
type DynamicCollidable = (CollisionModule, BoundingBox, Position, Velocity, Entity)

type CollisionInfo = (CollisionType, CollisionLayer)
type CollisionMap = Map.Map Entity [CollisionInfo]
type Collision = (To, From, CollisionType, CollisionLayer)

-- data SensorDirection =
--     Sensor'Top
--   | Sensor'Left
--   | Sensor'Right
--   | Sensor'Bottom
--   deriving Show

-- data SensorCollision = SensorCollision
--   { plane = SensorDirection
--   , side  = SensorDirection }
--   deriving Show

-- data RaycastHit = RaycastHit
--  { distance :: Unit
--  , fraction :: Double
--  , normal   :: V2 Unit }
--  deriving Show


determineCollisionType :: DynamicCollidable -> Collidable -> Collision
determineCollisionType (_, BoundingBox bb1, Position p1, v@(Velocity v'), e1)
                       (CollisionModule cLayer, BoundingBox bb2, Position p2, e2) =
  let box1 = AABB { center = p1, dims = bb1 }
      box2 = AABB { center = p2, dims = bb2 }
      (collisionTime, normal) = sweepAABB v box1 box2
      (pVec, pNormal) = penetrationVector box1 box2
      pVector = PenetrationVector $ pVec * (toVector pNormal)
      lowCollisionTime = ((coerce collisionTime :: Double) * frameDeltaSeconds) < 0.00005
      noPenetration = (abs <$> (coerce pVector :: V2 Unit)) == V2 0 0
      hasZeroNormal = normal == NoneNormal
      toEvent t = (To e1, From e2, t, cLayer)
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
addToCollisionMap (To e1, From e2, collisionType, cLayer) m =
  case collisionType of
    NoCollision                    -> m
    SimpleCollision (pVec, normal) ->
      let reverseVector = PenetrationVector $ negate <$> (coerce pVec :: V2 Unit)
          reverseNormal = inverseNormal normal
      in Map.insertWith (++) e2 [(SimpleCollision (reverseVector, reverseNormal), cLayer)] $
           Map.insertWith (++) e1 [(collisionType, cLayer)] m
    SweptCollision (cTime, normal) ->
      let reverseNormal = inverseNormal normal
          neutralVector = PenetrationVector $ V2 0 0
      in Map.insertWith (++) e2 [(SimpleCollision (neutralVector, reverseNormal), cLayer)] $
           Map.insertWith (++) e1 [(collisionType, cLayer)] m


isLegalCollision :: Entity -> AABB -> CollisionLayer -> Collidable -> Bool
isLegalCollision e box cLayer1 c@(CollisionModule cLayer2, _, _, _) =
     (areLayersCollidable cLayer1 cLayer2)
  && (inNarrowPhase e box c)

processCollidable :: [Collidable] -> DynamicCollidable -> [Collision]
processCollidable allCollidables
                  cm@( CollisionModule cLayer
                     , bb@(BoundingBox bb')
                     , p@(Position p')
                     , v@(Velocity v')
                     , entity ) =
  let sweptBox = broadPhaseAABB bb p v
      actives  = filter (isLegalCollision entity sweptBox cLayer) allCollidables
      -- if we have no swept phase collisions
  in if (length actives == 0)
     then [(To entity, From entity, NoCollision, CL'EmptyLayer)]
     else map (determineCollisionType cm) actives


-- post collection updates
-- util
getCollisions :: CollisionMap -> Entity -> [CollisionInfo]
getCollisions cm e = case (cm !? e) of Just c  -> c
                                       Nothing -> []

-- jump
stepJump' :: CollisionMap -> (CollisionModule, Jump, Entity) -> (Jump, [QueueEvent])
stepJump' cm (_, jumpState, e) =
  let collisions = getCollisions cm e
  in if (length collisions == 0)
     then if onGround jumpState
          then (jumpState { onGround = False, requested = False }, [])
          else (jumpState, [])
     else foldr stepJumpState (jumpState, []) collisions
     where
       landingEvent = AudioSystemEvent (e, Player'SFX'Land, Audio'PlayOrSustain)
       stepJumpState (ct, cl) (j, qs) =
             if (normal == BottomNormal)
             then if not $ onGround j
                  then (j { onGround = True }, qs ++ [landingEvent])
                  else (j, qs)
             else (jumpState, qs)
             where normal = case ct of SimpleCollision (_, normal) -> normal
                                       SweptCollision  (_, normal) -> normal


-- speed
stepSpeed :: CollisionInfo -> Velocity -> Velocity
stepSpeed (SimpleCollision (pVec, normal), _) v = resolveNormalVelocity v pVec normal
stepSpeed (SweptCollision (cTime, normal), _) v = resolveBaseCollision (cTime, normal) v

stepCollisionSpeed :: CollisionMap -> (CollisionModule, Velocity, Entity) -> Velocity
stepCollisionSpeed cm (_, v, e) =
  let collisions = getCollisions cm e
  in if (length collisions == 0)
     then v
     else foldr stepSpeed v collisions


-- position
stepPosition :: CollisionInfo -> (Position, Velocity) -> (Position, Velocity)
stepPosition (SimpleCollision (pVec, normal), _)
             (Position p, v@(Velocity v')) =
  (Position $ p + (coerce pVec :: V2 Unit), v)
stepPosition (SweptCollision (CollisionTime cTime, normal), _)
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

-- dispatch raycast debug event also
resolveXTiles :: TileMap -> DynamicCollidable -> ((Velocity, Position), [QueueEvent])
resolveXTiles tMap
            cm@( CollisionModule cLayer
               , bb@(BoundingBox bb')
               , p@(Position p')
               , v@(Velocity v'@(V2 vx vy))
               , entity ) =
  let box                = AABB { center = p', dims = bb' }
      V2 xOrigin yOrigin =
        if vx > 0
        -- pick moving edge & cast from 1 pixel away
        then aabbMax        box + V2   onePixel  onePixel
        else aabbBottomLeft box + V2 (-onePixel) onePixel
      V2 xFinal yFinal   =
        if vx > 0
        then aabbMin      box + V2   onePixel  (-onePixel)
        else aabbTopRight box + V2 (-onePixel) (-onePixel)
      tiles              =
        (raycastAlongX tMap (Position $ V2 xOrigin yOrigin) v)
        ++
        (raycastAlongX tMap (Position $ V2 xFinal  yFinal ) v)
  in ((v, p), [])
  -- in if (length tiles == 0)
  --    then ((v, Position $ p' + ((v' ^* Unit frameDeltaSeconds))), [])
  --    else ((Velocity $ v' * V2 0 1, p), [])

-- whole system
stepCollisionSystem :: [QueueEvent] -> System' [QueueEvent]
stepCollisionSystem events = do
  cEvents <- emap $ (resolveXTiles basicTilemap)
  -- get all entities with a collision module
  allCollidables    <- getAll :: System' [Collidable]
  movingCollidables <- getAll :: System' [DynamicCollidable]
  -- compute collisions, insert in entity-key collision-value map
  let collisions   = concat $ map (processCollidable allCollidables) movingCollidables
      collisionMap = foldr addToCollisionMap Map.empty collisions
  -- process CollisionModule related components
  cmap $ (stepCollisionSpeed    collisionMap)
  cmap $ \(CollisionModule _, Position p, Velocity v) ->
    Position $ p + (v ^* Unit frameDeltaSeconds)
  cmap $ (stepCollisionPosition collisionMap)
  jEvents <- emap $ (stepJump' collisionMap)
  return $ events ++ cEvents ++ jEvents
