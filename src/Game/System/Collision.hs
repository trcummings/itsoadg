module Game.System.Collision where

import           Linear (V2(..), (^*), (*^), (^/), dot, _x, _y)
import           Apecs (Entity, cmap, cmapM_, set, proxy, getAll, get, exists)
import           Data.Coerce (coerce)
import           Data.Map ((!?))
import           Data.Maybe (catMaybes)
import           Data.List (partition)
import qualified Data.Map as Map (Map, insertWith, empty)
import           Control.Lens
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
  , TileType(..)
  , RaycastHit(..)
  , Axis(..)
  , SensorDirection(..)
  , CollisionLayerType(..) )
import           Game.Wrapper.Apecs (emap)
import           Game.Util.Constants (frameDeltaSeconds, onePixel)
import           Game.Util.TileMap
  ( basicTilemap
  , getTileTypeAt
  -- , raycastAlongX
  -- , raycastAlongY
  , getIntersectingTiles )
import           Game.Util.Raycast
  ( raycast
  , getRaycastDir
  , getRaycastLength
  , getRaycastPos
  , getRaycastPosOffset )
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
  , areLayersCollidable
  , getCollisionType )

type Collidable = (CollisionModule, BoundingBox, Position, Entity)
type DynamicCollidable = (CollisionModule, BoundingBox, Position, Velocity, Entity)

-- type CollisionInfo = (CollisionType, CollisionLayer)
-- type CollisionMap = Map.Map Entity [CollisionInfo]
-- type Collision = (To, From, CollisionType, CollisionLayer)

-- determineCollisionType :: DynamicCollidable -> Collidable -> Collision
-- determineCollisionType (_, BoundingBox bb1, Position p1, v@(Velocity v'), e1)
--                        (CollisionModule cLayer, BoundingBox bb2, Position p2, e2) =
--   let box1 = AABB { center = p1, dims = bb1 }
--       box2 = AABB { center = p2, dims = bb2 }
--       (collisionTime, normal) = sweepAABB v box1 box2
--       (pVec, pNormal) = penetrationVector box1 box2
--       pVector = PenetrationVector $ pVec * (toVector pNormal)
--       lowCollisionTime = ((coerce collisionTime :: Double) * frameDeltaSeconds) < 0.00005
--       noPenetration = (abs <$> (coerce pVector :: V2 Unit)) == V2 0 0
--       hasZeroNormal = normal == NoneNormal
--       toEvent t = (To e1, From e2, t, cLayer)
--       useSimpleResolution =
--             hasZeroNormal
--         ||  lowCollisionTime
--         || (hasZeroNormal && noPenetration)
--   -- in toEvent $ SweptCollision (collisionTime, normal)
--   in if (useSimpleResolution)
--      then
--        let adjustedV = resolveNormalVelocity v pVector normal
--            -- adjustedP = Position $ p1 + v' ^* Unit frameDeltaSeconds
--            -- adjustedBBB = broadPhaseAABB (BoundingBox bb1) adjustedP adjustedV
--            adjustedBBB = broadPhaseAABB (BoundingBox bb1) (Position p1) adjustedV
--            willNotEscape = aabbCheck adjustedBBB box2
--                  in if (hasZeroNormal && not noPenetration && willNotEscape)
--           then toEvent $ SimpleCollision (pVector, pNormal)
--           else toEvent NoCollision
--      else toEvent $ SweptCollision (collisionTime, normal)


-- addToCollisionMap :: Collision -> CollisionMap -> CollisionMap
-- addToCollisionMap (To e1, From e2, collisionType, cLayer) m =
--   case collisionType of
--     NoCollision                    -> m
--     SimpleCollision (pVec, normal) ->
--       let reverseVector = PenetrationVector $ negate <$> (coerce pVec :: V2 Unit)
--           reverseNormal = inverseNormal normal
--       in Map.insertWith (++) e2 [(SimpleCollision (reverseVector, reverseNormal), cLayer)] $
--            Map.insertWith (++) e1 [(collisionType, cLayer)] m
--     SweptCollision (cTime, normal) ->
--       let reverseNormal = inverseNormal normal
--           neutralVector = PenetrationVector $ V2 0 0
--       in Map.insertWith (++) e2 [(SimpleCollision (neutralVector, reverseNormal), cLayer)] $
--            Map.insertWith (++) e1 [(collisionType, cLayer)] m


isLegalCollision :: Entity -> AABB -> CollisionLayer -> Collidable -> Bool
isLegalCollision e box cLayer1 c@(CollisionModule cLayer2 _, _, _, _) =
     (areLayersCollidable cLayer1 cLayer2)
  && (inNarrowPhase e box c)

-- type LayerMap = Map.Map CollisionLayer [(AABB, LayerInfo)]

-- addToLayerMap :: (CollisionLayer, AABB, LayerInfo) -> LayerMap -> LayerMap
-- addToLayerMap (cLayer, box, info) = Map.insertWith (++) cLayer [(box, info)]
type LayerInfo = Either Entity TileType
type CollisionInfo = (AABB, (CollisionLayer, LayerInfo))

tileToLayerInfo :: (TileType, V2 Unit) -> CollisionInfo
tileToLayerInfo (t, p) = ( AABB { center = p, dims = V2 1 1 }
                         , (CL'Tile, Right t) )

collidableToLayerInfo :: Collidable -> CollisionInfo
collidableToLayerInfo (CollisionModule cl _, BoundingBox bb, Position p, e) =
  ( AABB { center = p, dims = bb }, (cl, Left e) )

isSolidInteraction :: CollisionLayer -> CollisionInfo -> Bool
isSolidInteraction cLayer1 (_, (cLayer2, _)) =
  getCollisionType cLayer1 cLayer2 == CLT'Solid

processCollidable :: TileMap
                  -> [Collidable]
                  -> DynamicCollidable
                  -> (Velocity, [QueueEvent])
processCollidable tileMap
                  allCollidableEntities
                  ( CollisionModule cLayer _
                  , bb@(BoundingBox bb')
                  , p@(Position p')
                  , v@(Velocity v')
                  , entity ) =
  let box                 = AABB { center = p', dims = bb' }
      sweptBox            = broadPhaseAABB bb p v
      sweptTiles          = map tileToLayerInfo $
        if (areLayersCollidable cLayer CL'Tile)
        then getIntersectingTiles tileMap sweptBox
        else []
      sweptEntities       = map collidableToLayerInfo $
        filter (isLegalCollision entity sweptBox cLayer) allCollidableEntities
      -- partition collidable entities into solids, which force physics resolution
      -- and produce effects, & nonsolids, which produce effects
      (solids, nonsolids) =
        partition (isSolidInteraction cLayer) (sweptTiles ++ sweptEntities)
      -- process horizontal collisions first
  in ( (processRay X Sensor'Top    box solids)
     . (processRay X Sensor'Bottom box solids)
     -- then process vertical collisions
     . (processRay Y Sensor'Right  box solids)
     . (processRay Y Sensor'Left   box solids) ) (v, [])

processRay :: Axis
           -> SensorDirection
           -> AABB
           -> [CollisionInfo]
           -> (Velocity, [QueueEvent])
           -> (Velocity, [QueueEvent])
processRay axis sDir box tiles (v@(Velocity v'@(V2 vx vy)), qs) =
  let dir        = getRaycastDir    axis v
      castLength = getRaycastLength v dir
      pos        = getRaycastPos sDir v box
      offset     = getRaycastPosOffset sDir v
      mainV      = if axis == X then V2 1 0 else V2 0 1
      secondV    = if axis == X then V2 0 1 else V2 1 0
  in if ((axis == X && v' ^. _x == 0) || (axis == Y && v' ^. _y == 0))
     then (v, qs)
     else case (raycast (pos + offset) castLength tiles) of
            Nothing        -> (v, qs)
            Just (hit, tt) ->
              let newV = distance hit - offset
              in ( Velocity $ (newV * mainV) + (v' * secondV)
                 , qs ++ [CollisionSystemEvent (hit, (pos + offset, castLength))] )

-- -- post collection updates
-- -- util
-- getCollisions :: CollisionMap -> Entity -> [CollisionInfo]
-- getCollisions cm e = case (cm !? e) of Just c  -> c
--                                        Nothing -> []

-- -- jump
-- stepJump' :: CollisionMap -> (CollisionModule, Jump, Entity) -> (Jump, [QueueEvent])
-- stepJump' cm (_, jumpState, e) =
--   let collisions = getCollisions cm e
--   in if (length collisions == 0)
--      then if onGround jumpState
--           then (jumpState { onGround = False, requested = False }, [])
--           else (jumpState, [])
--      else foldr stepJumpState (jumpState, []) collisions
--      where
--        landingEvent = AudioSystemEvent (e, Player'SFX'Land, Audio'PlayOrSustain)
--        stepJumpState (ct, cl) (j, qs) =
--              if (normal == BottomNormal)
--              then if not $ onGround j
--                   then (j { onGround = True }, qs ++ [landingEvent])
--                   else (j, qs)
--              else (jumpState, qs)
--              where normal = case ct of SimpleCollision (_, normal) -> normal
--                                        SweptCollision  (_, normal) -> normal


-- -- speed
-- stepSpeed :: CollisionInfo -> Velocity -> Velocity
-- stepSpeed (SimpleCollision (pVec, normal), _) v = resolveNormalVelocity v pVec normal
-- stepSpeed (SweptCollision (cTime, normal), _) v = resolveBaseCollision (cTime, normal) v

-- stepCollisionSpeed :: CollisionMap -> (CollisionModule, Velocity, Entity) -> Velocity
-- stepCollisionSpeed cm (_, v, e) =
--   let collisions = getCollisions cm e
--   in if (length collisions == 0)
--      then v
--      else foldr stepSpeed v collisions

-- -- position
-- stepPosition :: CollisionInfo -> (Position, Velocity) -> (Position, Velocity)
-- stepPosition (SimpleCollision (pVec, normal), _)
--              (Position p, v@(Velocity v')) =
--   (Position $ p + (coerce pVec :: V2 Unit), v)
-- stepPosition (SweptCollision (CollisionTime cTime, normal), _)
--              (Position p, v@(Velocity v')) =
--   let cTime' = (coerce cTime :: Double) * frameDeltaSeconds
--       -- lowCollisionTime = cTime' < 0.00005
--       -- time = if (lowCollisionTime) then frameDeltaSeconds else cTime'
--   in (Position $ p + v' ^* Unit cTime', v)

-- stepCollisionPosition :: CollisionMap
--                       -> (CollisionModule, Velocity, Position, Entity)
--                       -> Position
-- stepCollisionPosition cm (_, v@(Velocity v'), p@(Position p'), e) =
--   let collisions = getCollisions cm e
--   in if (length collisions == 0)
--      then Position $ p' + (v' ^* Unit frameDeltaSeconds)
--      else fst $ foldr stepPosition (p, v) collisions
-- stepHardFlow :: (HardFlow, CollisionModule) -> 

-- whole system
stepCollisionSystem :: [QueueEvent] -> System' [QueueEvent]
stepCollisionSystem events = do
  -- clear collision layer map from collision modules
  cmap $ \(cm@(CollisionModule _ _)) -> cm { layerCollisions = Map.empty }
  -- get all entities with a collision module
  allCollidables    <- getAll :: System' [Collidable]
  movingCollidables <- getAll :: System' [DynamicCollidable]
  -- -- compute collisions, insert in entity-key collision-value map
  nEvents <- emap $ (processCollidable basicTilemap allCollidables)
  -- liftIO $ putStrLn $ show nEvents
  cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
    Position $ p + (v ^* Unit frameDeltaSeconds)
  -- cmap $ \(HardFlow, cm@(CollisionModule _ _)) ->
  -- jEvents <- emap $ (stepJump' collisionMap)
  -- return $ events ++ jEvents
  return events
