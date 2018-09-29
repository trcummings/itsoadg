module Game.System.Collision where

import           Control.Monad (when)
import           Linear (V3(..))
import           Apecs

import           Game.World.TH (ECS)
import           Game.Types
  ( CollisionModule(..)
  , Collider(..)
  , Position3D(..)
  , Player
  , SimpleCube
  , AABB(..)
  )

type Collidable = (CollisionModule, Position3D)

toBox :: (Collidable, Entity) -> (AABB, Entity)
toBox ((cm, Position3D cPos), ety) =
  let (BoxCollider dims) = _collider cm
      minPt = cPos - (dims / 2)
      maxPt = cPos + (dims / 2)
      aabb  = AABB { _bbMin = minPt
                   , _bbMax = maxPt }
  in (aabb, ety)

aabbCheck :: AABB -> AABB -> Bool
aabbCheck a b =
  let (V3 aMinX aMinY aMinZ) = _bbMin a
      (V3 aMaxX aMaxY aMaxZ) = _bbMax a
      (V3 bMinX bMinY bMinZ) = _bbMin b
      (V3 bMaxX bMaxY bMaxZ) = _bbMax b
  in   (aMinX <= bMaxX && aMaxX >= bMinX)
    && (aMinY <= bMaxY && aMaxY >= bMinY)
    && (aMinZ <= bMaxZ && aMaxZ >= bMinZ)


checkForCollision :: (AABB, Entity) -> (AABB, Entity) -> Bool
checkForCollision (box1, ety1) (box2, ety2) =
  (not $ ety1 == ety2) && (aabbCheck box1 box2)

processCollidable :: [(AABB, Entity)] -> (Collidable, Entity) -> CollisionModule
processCollidable candidates candidate@((cm, _), _) =
  if any (checkForCollision $ toBox candidate) candidates
  then cm { _hasCollision = True }
  else cm

stepCollisionSystem :: ECS ()
stepCollisionSystem = do
  cmap $ \(cm :: CollisionModule) -> cm { _hasCollision = False }
  collidables <- getAll :: ECS [(Collidable, Entity)]
  let candidates = map toBox collidables
  cmap $ processCollidable candidates
  -- clear collision layer map from collision modules
  -- cmap $ \(cm@(CollisionModule _ _)) -> cm { layerCollisions = [] }
  -- -- move x first
  -- cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
  --   Position $ p + ((v * V2 1 0) ^* Unit frameDeltaSeconds)
  -- -- detect x collisions
  -- -- get all collidable objects
  -- allCollidables1 <- getAll :: System' [Collidable]
  -- -- resolve x collisions

  -- -- move y second
  -- cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
  --   Position $ p + ((v * V2 0 1) ^* Unit frameDeltaSeconds)
  -- -- resolve y

  -- get all entities with a collision module
  -- allCollidables :: [Collidable] <- getAll
  -- -- compute collisions, insert in entity-key collision-value map
  -- cmap $ (processCollidable basicTilemap allCollidables)
  -- cmapM_ $ \(Player _, cm@(CollisionModule _ _)) ->
  --   when ((length $ layerCollisions cm) >= 2) $
  --     liftIO $ putStrLn $ "Player: " ++ show cm
  -- cmapM_ $ \(HardFlow, cm@(CollisionModule _ _)) -> liftIO $ putStrLn $ "HF: " ++ show cm
  -- cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
  --   Position $ p + (v ^* Unit frameDeltaSeconds)
  -- cmap $ \(HardFlow, cm@(CollisionModule _ _)) ->
  -- jEvents <- qmap $ stepJump'
  -- return $ events ++ jEvents





-- import           Linear (V2(..), (^*), (*^), (^/), dot, _x, _y)
-- import           Apecs (Entity, proxy)
-- import           Data.Coerce (coerce)
-- import           Data.Map ((!?))
-- import           Data.Maybe (catMaybes)
-- import           Data.List (partition)
-- import qualified Data.Map as Map (Map, insertWith, empty)
-- import           Control.Lens
-- import           Control.Monad (when)
-- import           Control.Monad.Extra (partitionM)
-- import           Control.Monad.IO.Class (liftIO)
--
--
-- import           Game.Types
--   ( Unit(..)
--   , Position(..)
--   , Velocity(..)
--   , CollisionNormal(..)
--   , CollisionTime(..)
--   , CollisionType(..)
--   , PenetrationVector(..)
--   , CollisionModule(..)
--   , CollisionLayer(..)
--   , BoxEntity(..)
--   , BoundingBox(..)
--   , AABB(..), dims, center
--   , Jump(..)
--   , Player(..)
--   , HardFlow(..)
--   , QueueEvent(..)
--   , Audio'Command(..)
--   , Player'SFX'Key(..)
--   , To(..)
--   , From(..)
--   , TileMap(..)
--   , TileType(..)
--   , RaycastHit(..)
--   , Axis(..)
--   , SensorDirection(..)
--   , CollisionLayerType(..) )
-- import           Game.Wrapper.Apecs (Apecs(..))
-- import           Game.Util.Constants (frameDeltaSeconds, onePixel)
-- import           Game.Util.TileMap
--   ( basicTilemap
--   , getTileTypeAt
--   -- , raycastAlongX
--   -- , raycastAlongY
--   , getIntersectingTiles )
-- import           Game.Util.Raycast
--   ( raycast
--   , getRaycastDir
--   , getRaycastLength
--   , getRaycastPos
--   -- , getRaycastPosOffset
--   , shrinkBox )
-- import           Game.Util.AABB
--   ( aabbCheck
--   , sweepAABB
--   , aabbMin
--   , aabbMax
--   , aabbBottomLeft
--   , aabbTopRight
--   , penetrationVector
--   , broadPhaseAABB
--   , inNarrowPhase )
-- import           Game.Util.Collision
--   ( toVector
--   , resolveBaseCollision
--   , resolveNormalVelocity
--   , inverseNormal
--   , areLayersCollidable
--   , getCollisionType )
--
-- type Collidable = (CollisionModule, BoundingBox, Position, Entity)
-- type DynamicCollidable = (CollisionModule, BoundingBox, Position, Velocity, Entity)
--
-- -- type CollisionInfo = (CollisionType, CollisionLayer)
-- -- type CollisionMap = Map.Map Entity [CollisionInfo]
-- -- type Collision = (To, From, CollisionType, CollisionLayer)
--
-- -- determineCollisionType :: DynamicCollidable -> Collidable -> Collision
-- -- determineCollisionType (_, BoundingBox bb1, Position p1, v@(Velocity v'), e1)
-- --                        (CollisionModule cLayer, BoundingBox bb2, Position p2, e2) =
-- --   let box1 = AABB { center = p1, dims = bb1 }
-- --       box2 = AABB { center = p2, dims = bb2 }
-- --       (collisionTime, normal) = sweepAABB v box1 box2
-- --       (pVec, pNormal) = penetrationVector box1 box2
-- --       pVector = PenetrationVector $ pVec * (toVector pNormal)
-- --       lowCollisionTime = ((coerce collisionTime :: Double) * frameDeltaSeconds) < 0.00005
-- --       noPenetration = (abs <$> (coerce pVector :: V2 Unit)) == V2 0 0
-- --       hasZeroNormal = normal == NoneNormal
-- --       toEvent t = (To e1, From e2, t, cLayer)
-- --       useSimpleResolution =
-- --             hasZeroNormal
-- --         ||  lowCollisionTime
-- --         || (hasZeroNormal && noPenetration)
-- --   -- in toEvent $ SweptCollision (collisionTime, normal)
-- --   in if (useSimpleResolution)
-- --      then
-- --        let adjustedV = resolveNormalVelocity v pVector normal
-- --            -- adjustedP = Position $ p1 + v' ^* Unit frameDeltaSeconds
-- --            -- adjustedBBB = broadPhaseAABB (BoundingBox bb1) adjustedP adjustedV
-- --            adjustedBBB = broadPhaseAABB (BoundingBox bb1) (Position p1) adjustedV
-- --            willNotEscape = aabbCheck adjustedBBB box2
-- --                  in if (hasZeroNormal && not noPenetration && willNotEscape)
-- --           then toEvent $ SimpleCollision (pVector, pNormal)
-- --           else toEvent NoCollision
-- --      else toEvent $ SweptCollision (collisionTime, normal)
--
--
-- -- addToCollisionMap :: Collision -> CollisionMap -> CollisionMap
-- -- addToCollisionMap (To e1, From e2, collisionType, cLayer) m =
-- --   case collisionType of
-- --     NoCollision                    -> m
-- --     SimpleCollision (pVec, normal) ->
-- --       let reverseVector = PenetrationVector $ negate <$> (coerce pVec :: V2 Unit)
-- --           reverseNormal = inverseNormal normal
-- --       in Map.insertWith (++) e2 [(SimpleCollision (reverseVector, reverseNormal), cLayer)] $
-- --            Map.insertWith (++) e1 [(collisionType, cLayer)] m
-- --     SweptCollision (cTime, normal) ->
-- --       let reverseNormal = inverseNormal normal
-- --           neutralVector = PenetrationVector $ V2 0 0
-- --       in Map.insertWith (++) e2 [(SimpleCollision (neutralVector, reverseNormal), cLayer)] $
-- --            Map.insertWith (++) e1 [(collisionType, cLayer)] m
--
--
-- isLegalCollision :: Entity -> AABB -> CollisionLayer -> Collidable -> Bool
-- isLegalCollision e box cLayer1 c@(CollisionModule cLayer2 _, _, _, _) =
--      (areLayersCollidable cLayer1 cLayer2)
--   && (inNarrowPhase e box c)
--
-- -- type LayerMap = Map.Map CollisionLayer [(AABB, LayerInfo)]
--
-- -- addToLayerMap :: (CollisionLayer, AABB, LayerInfo) -> LayerMap -> LayerMap
-- -- addToLayerMap (cLayer, box, info) = Map.insertWith (++) cLayer [(box, info)]
-- type LayerInfo = Either Entity TileType
-- type CollisionInfo = (AABB, (CollisionLayer, LayerInfo))
--
-- tileToLayerInfo :: (TileType, V2 Unit) -> CollisionInfo
-- tileToLayerInfo (t, p) = ( AABB { center = p, dims = V2 1 1 }
--                          , (CL'Tile, Right t) )
--
-- collidableToLayerInfo :: Collidable -> CollisionInfo
-- collidableToLayerInfo (CollisionModule cl _, BoundingBox bb, Position p, e) =
--   ( AABB { center = p, dims = bb }, (cl, Left e) )
--
-- isSolidInteraction :: CollisionLayer -> CollisionInfo -> Bool
-- isSolidInteraction cLayer1 (_, (cLayer2, _)) =
--   getCollisionType cLayer1 cLayer2 == CLT'Solid
--
--
--
-- getSweptCollidables :: TileMap -> [Collidable] -> DynamicCollidable -> [CollisionInfo]
-- getSweptCollidables tileMap allCollidableEntities (cm, bb, p, v, e) =
--   let sweptBox      = broadPhaseAABB bb p v
--       sweptTiles    = map tileToLayerInfo $
--         if (areLayersCollidable (layer cm) CL'Tile)
--         then getIntersectingTiles tileMap sweptBox
--         else []
--       sweptEntities = map collidableToLayerInfo $
--         filter (isLegalCollision e sweptBox (layer cm)) allCollidableEntities
--    in sweptTiles ++ sweptEntities
--
--
--
-- processCollidable :: TileMap
--                   -> [Collidable]
--                   -> DynamicCollidable
--                   -> (CollisionModule, Velocity, Position)
-- processCollidable tileMap
--                   allCollidableEntities
--                   dc@( cm@(CollisionModule cLayer _)
--                      , bb@(BoundingBox bb')
--                      , p@(Position p')
--                      , Velocity v
--                      , entity ) =
--   let sweptCollidables = getSweptCollidables tileMap allCollidableEntities dc
--       -- x axis
--       boxX                 = AABB { center = p', dims = bb' }
--       sweptBoxX            = broadPhaseAABB bb p (Velocity $ v * V2 1 0)
--       xCollidables = filter ((aabbCheck sweptBoxX) . fst) sweptCollidables
--       -- partition collidable entities into solids & nonsolids
--       (solidsX, nonsolidsX) =
--         partition (isSolidInteraction cLayer) xCollidables
--       -- resolve solid collision
--       (Velocity vX, cmX) = ( (processRay X Sensor'Top    boxX solidsX)
--                            . (processRay X Sensor'Bottom boxX solidsX) ) (Velocity v, cm)
--       -- create new swept aabb with resolved velocity to collect nonsolids
--       sweptBoxX' = broadPhaseAABB bb p (Velocity $ vX * V2 1 0)
--       nonsolidsX' = filter ((aabbCheck sweptBoxX') . fst) nonsolidsX
--       (_, cmX') = ( (processRay X Sensor'Top    boxX nonsolidsX')
--                   . (processRay X Sensor'Bottom boxX nonsolidsX') ) (Velocity vX, cmX)
--       pX = p' + ((vX * V2 1 0) ^* Unit frameDeltaSeconds)
--       -- y axis
--       boxY = AABB { center = pX, dims = bb' }
--       sweptBoxY = broadPhaseAABB bb (Position pX) (Velocity $ vX * V2 0 1)
--       yCollidables = filter ((aabbCheck sweptBoxY) . fst) sweptCollidables
--       (solidsY, nonsolidsY) =
--         partition (isSolidInteraction cLayer) yCollidables
--       (Velocity vY, cmY) = ( (processRay Y Sensor'Right boxY solidsY)
--                            . (processRay Y Sensor'Left  boxY solidsY) ) (Velocity vX, cmX)
--       pY = pX + ((vY * V2 0 1) ^* Unit frameDeltaSeconds)
--   in (cmY, Velocity vY, Position pY)
--
-- processRay :: Axis
--            -> SensorDirection
--            -> AABB
--            -> [CollisionInfo]
--            -> (Velocity, CollisionModule)
--            -> (Velocity, CollisionModule)
-- processRay axis sDir box tiles (v@(Velocity v'@(V2 vx vy)), cm) =
--       -- direction vector for raycast
--   let dir        = getRaycastDir axis v
--       pos        = getRaycastPos sDir v box
--       offset     = dir * (V2 onePixel (-onePixel))
--       castLength = getRaycastLength v dir
--       mainV      = if axis == X then V2 1 0 else V2 0 1
--       secondV    = if axis == X then V2 0 1 else V2 1 0
--   in if ((axis == X && v' ^. _x == 0) || (axis == Y && v' ^. _y == 0))
--      then (v, cm)
--      else case (raycast pos castLength tiles) of
--             Nothing              -> (v, cm)
--             Just (hit, (cl, li)) ->
--               let newV = distance hit
--               in ( Velocity $ (newV * mainV) + (v' * secondV)
--                  , cm { layerCollisions = layerCollisions cm ++ [(cl, hit, li)] } )
--
-- -- -- post collection updates
-- -- -- util
-- -- getCollisions :: CollisionMap -> Entity -> [CollisionInfo]
-- -- getCollisions cm e = case (cm !? e) of Just c  -> c
-- --                                        Nothing -> []
--
-- -- -- jump
-- stepJump' :: (CollisionModule, Jump, Entity) -> (Jump, [QueueEvent])
-- stepJump' (cm, jumpState, e) =
--   let collisions = layerCollisions cm
--   in if (length collisions == 0)
--      then if onGround jumpState
--           then (jumpState { onGround = False, requested = False }, [])
--           else (jumpState, [])
--      else foldr stepJumpState (jumpState, []) collisions
--      where
--        landingEvent = AudioSystemEvent (e, Player'SFX'Land, Audio'PlayOrSustain)
--        stepJumpState (cl, hit, li) (j, qs) =
--              if (normal hit == V2 0 1)
--              then if not $ onGround j
--                   then (j { onGround = True }, qs ++ [landingEvent])
--                   else (j, qs)
--              else (jumpState, qs)
--
-- -- -- speed
-- -- stepSpeed :: CollisionInfo -> Velocity -> Velocity
-- -- stepSpeed (SimpleCollision (pVec, normal), _) v = resolveNormalVelocity v pVec normal
-- -- stepSpeed (SweptCollision (cTime, normal), _) v = resolveBaseCollision (cTime, normal) v
--
-- -- stepCollisionSpeed :: CollisionMap -> (CollisionModule, Velocity, Entity) -> Velocity
-- -- stepCollisionSpeed cm (_, v, e) =
-- --   let collisions = getCollisions cm e
-- --   in if (length collisions == 0)
-- --      then v
-- --      else foldr stepSpeed v collisions
--
-- -- -- position
-- -- stepPosition :: CollisionInfo -> (Position, Velocity) -> (Position, Velocity)
-- -- stepPosition (SimpleCollision (pVec, normal), _)
-- --              (Position p, v@(Velocity v')) =
-- --   (Position $ p + (coerce pVec :: V2 Unit), v)
-- -- stepPosition (SweptCollision (CollisionTime cTime, normal), _)
-- --              (Position p, v@(Velocity v')) =
-- --   let cTime' = (coerce cTime :: Double) * frameDeltaSeconds
-- --       -- lowCollisionTime = cTime' < 0.00005
-- --       -- time = if (lowCollisionTime) then frameDeltaSeconds else cTime'
-- --   in (Position $ p + v' ^* Unit cTime', v)
--
-- -- stepCollisionPosition :: CollisionMap
-- --                       -> (CollisionModule, Velocity, Position, Entity)
-- --                       -> Position
-- -- stepCollisionPosition cm (_, v@(Velocity v'), p@(Position p'), e) =
-- --   let collisions = getCollisions cm e
-- --   in if (length collisions == 0)
-- --      then Position $ p' + (v' ^* Unit frameDeltaSeconds)
-- --      else fst $ foldr stepPosition (p, v) collisions
-- -- stepHardFlow :: (HardFlow, CollisionModule) ->
--
-- -- whole system
-- stepCollisionSystem :: Apecs m => [QueueEvent] -> m [QueueEvent]
-- stepCollisionSystem events = do
--   -- clear collision layer map from collision modules
--   cmap $ \(cm@(CollisionModule _ _)) -> cm { layerCollisions = [] }
--   -- -- move x first
--   -- cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
--   --   Position $ p + ((v * V2 1 0) ^* Unit frameDeltaSeconds)
--   -- -- detect x collisions
--   -- -- get all collidable objects
--   -- allCollidables1 <- getAll :: System' [Collidable]
--   -- -- resolve x collisions
--
--   -- -- move y second
--   -- cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
--   --   Position $ p + ((v * V2 0 1) ^* Unit frameDeltaSeconds)
--   -- -- resolve y
--
--   -- get all entities with a collision module
--   allCollidables :: [Collidable] <- getAll
--   -- -- compute collisions, insert in entity-key collision-value map
--   cmap $ (processCollidable basicTilemap allCollidables)
--   -- cmapM_ $ \(Player _, cm@(CollisionModule _ _)) ->
--   --   when ((length $ layerCollisions cm) >= 2) $
--   --     liftIO $ putStrLn $ "Player: " ++ show cm
--   -- cmapM_ $ \(HardFlow, cm@(CollisionModule _ _)) -> liftIO $ putStrLn $ "HF: " ++ show cm
--   -- cmap $ \(CollisionModule _ _, Position p, Velocity v) ->
--   --   Position $ p + (v ^* Unit frameDeltaSeconds)
--   -- cmap $ \(HardFlow, cm@(CollisionModule _ _)) ->
--   jEvents <- qmap $ stepJump'
--   return $ events ++ jEvents
