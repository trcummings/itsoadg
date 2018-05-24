{-# LANGUAGE FlexibleContexts #-}

module Game.Physics where

import qualified SDL
import           Control.Monad (when)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (partition)
import           Linear (V2(..), V4(..), (^*), (*^))
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
import           Game.Collision
  ( AABB(..), center, dims
  , Collision(..)
  , CNormal(..)
  , toVector
  , aabbCheck
  , sweepAABB
  , broadPhaseAABB )
import           Game.Constants
  ( Unit(..)
  , dT
  , dTinSeconds
  , playerSpeed
  , maxSpeed
  , gravity
  , screenWidth
  , screenHeight
  , maxSpeed
  , playerSpeed )
import           Game.Types
  ( Player(..)
  , Jump(..), jumpCommandReceived, isJumping
  , Acceleration(..)
  , Position(..)
  , Camera(..), size, ppos
  , CameraTarget(..)
  , Velocity(..)
  , Friction(..)
  , BoundingBox(..)
  , Gravity(..)
  , PhysicsTime(..), time, accum
  , GlobalTime(..) )


-- constant acceleration
bumpX x = cmap $ \(Player, Acceleration (V2 _ y)) -> Acceleration $ V2 x y

setJump x = cmap $ \(Player, Jump _ ij) ->
  Jump { jumpCommandReceived = x, isJumping = ij }

-- move left
handleArrowEvent SDL.KeycodeA SDL.Pressed  = bumpX (-playerSpeed)
handleArrowEvent SDL.KeycodeA SDL.Released = bumpX (Unit 0)
-- move right
handleArrowEvent SDL.KeycodeD SDL.Pressed  = bumpX playerSpeed
handleArrowEvent SDL.KeycodeD SDL.Released = bumpX (Unit 0)
-- jump
handleArrowEvent SDL.KeycodeW SDL.Pressed  = setJump True
handleArrowEvent SDL.KeycodeW SDL.Released = setJump False
handleArrowEvent _ _ = return ()

handleEvent :: SDL.Event -> System' ()
handleEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      handleArrowEvent keyCode motion
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent
    _ -> return ()

-- handleFloor :: Entity -> System' ()
-- handleFloor e = do
--   (_, Friction f, Position p') <- get e :: System' (Floor, Friction, Position)

--   cmap $ \(Player, Velocity (V2 vx _)) ->
--     -- apply horizontal friction
--     let vx' = (vx * Unit f)
--     -- set vertical velocity to 0
--         vy' = 0
--     in Velocity $ V2 vx' vy'

--   -- clear isJumping
--   cmap $ \(Player, Jump _ _) ->
--     Jump { jumpCommandReceived = False, isJumping = False }


type BoxEntity = (BoundingBox, Position, Entity)

hasVelComponent :: BoxEntity -> System' Bool
hasVelComponent (_, _, e) = do
  hasVelocity <- exists e (proxy :: Velocity)
  return hasVelocity

inNarrowPhase :: Entity -> AABB -> BoxEntity -> Bool
inNarrowPhase e sweptBox (BoundingBox bb, Position p, e') =
  (not $ e' == e) && (aabbCheck sweptBox $ AABB { center = p, dims = bb })

foldCollisions :: AABB
               -> Velocity
               -> [BoxEntity]
               -> [Collision]
               -> [Collision]
foldCollisions _   _ []                                   cls = cls
foldCollisions box v ((BoundingBox bb, Position p, e):cs) cls =
  let (collisionTime, normal) = sweepAABB v box $ AABB { center = p, dims = bb }
  in if (collisionTime >= 1)
     then cls
     else cls ++ [Collision collisionTime normal e]

handleBaseCollision :: Entity -> Collision -> System' ()
handleBaseCollision e c@(Collision collisionTime normal _) = do
  liftIO $ putStrLn $ show collisionTime ++ ", " ++ show normal
  ((Velocity v@(V2 vx vy)), Position p) <- get e :: System' (Velocity, Position)

  let p'            = p + (v ^* Unit collisionTime)
      remainingTime = 1 - collisionTime
      vNormal@(V2 normalX normalY) = toVector normal
      dotProd = ((vx * normalY) + (vy * normalX)) * Unit remainingTime
      v' = V2 (dotProd * normalY) (dotProd * normalX)
  liftIO $ putStrLn $ "Before: " ++ show p ++ ", " ++ show v
  liftIO $ putStrLn $ "After: "  ++ show p' ++ ", " ++ show v'
  set e (Position p', Velocity v')
  --    when isFloor $ (handleFloor e) ) es

handleFloor :: Entity -> Collision -> System' ()
handleFloor e c@(Collision _ normal e') = do
  hasFriction <- exists e' (proxy :: Friction)
  when hasFriction $ do
    (Velocity (V2 vx vy)) <- get e  :: System' (Velocity)
    (Friction f)          <- get e' :: System' (Friction)
    case normal of
      TopN    -> do
        set e (Velocity $ V2 (vx * Unit f) vy)
      BottomN -> do
        set e (Velocity $ V2 (vx * Unit f) vy)
      LeftN   -> do
        set e (Velocity $ V2 vx (vy * Unit f))
      RightN  -> do
        set e (Velocity $ V2 vx (vy * Unit f))
      _ -> return ()

handleCollision :: Entity -> Collision -> System' ()
handleCollision e c = do
  handleBaseCollision e c
  handleFloor e c

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
           box        = AABB { center = p', dims = bb' }
           collisions = foldCollisions box v actives []
       mapM_ (handleCollision entity) collisions
       -- when no collisions, update position and velocity normally
       when (length collisions == 0) $ do
         -- update position based on time and velocity
         let p'' = p' + (v' ^* Unit dTinSeconds)
         set entity (Position p'')
     ) dynamics

  -- -- resolve collisions
  -- cmapM_ $ \(Collisions es, e) -> do
  --   -- loop over each collision. if its a wall, if its a floor, else ignore
  --   mapM (\e -> do
  --    isFloor <- exists e (proxy :: (Floor, Position))
  --    when isFloor $ (handleFloor e) ) es

  -- -- clear remaining collisions
  -- cmap $ \(Collisions _) -> Collisions []



stepCamera :: System' ()
stepCamera = do
  -- update camera position based on target
  cmapM_ $ \(
      Camera s@(V2 cw ch) cp
    , CameraTarget e
    , Acceleration a
    , Position cpos
    , camera ) -> do

    (Position targetP) <- get e :: System' (Position)
        -- target x y based on camera size
    let txy = targetP - V2 (0.5 * cw) (0.6 * ch)
        -- camera acceleration towards target
        a'   = a + (txy - cpos)
        -- ppos with drag
        ppos' = cpos + (0.5 *^ (cp - cpos))
        -- verlet on cpos
        cpos' = cpos + (0.256 *^ a')
        -- differentiate to get new cpos
        d     = (2 *^ cpos') - ppos'
    -- set new values
    set camera (
        Camera { size = s, ppos = cpos' }
      , Acceleration $ V2 0 0
      , Position d )


clampVelocity :: Unit -> Unit
clampVelocity v =
  if (v > 0)
  then min v maxSpeed
  else max v (-maxSpeed)

runPhysics :: System' ()
runPhysics = do
  -- update acceleration based on gravity
  cmap $ \(Gravity, Acceleration (V2 x _)) -> Acceleration $ V2 x gravity

  -- update velocity based on acceleration
  cmap $ \(Acceleration a, Velocity v) ->
    let v' = (v + (a ^* Unit dTinSeconds))
    in Velocity $ clampVelocity <$> v'

  -- collisions
  handleCollisions

  -- jump!
  cmapM_ $ \(Player, Jump jcr ij, e) -> do
    Velocity v@(V2 vx vy) <- get e :: System' (Velocity)
    when (jcr && not ij) $ do
      set e ( Velocity $ V2 vx (vy - 20)
            , Jump { jumpCommandReceived = jcr, isJumping = True } )

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
