{-# LANGUAGE FlexibleContexts #-}

module Game.Physics where

import qualified SDL
import           Control.Monad (when, unless)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Map (member, insert, (!))
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
  , penetrationVector
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
  , playerSpeed
  , initialJumpVy
  , initialJumpG )
import           Game.Types
  ( Player(..)
  , Jump(..), buttonPressed, isJumping, isGrounded
  , Acceleration(..)
  , Position(..)
  , Camera(..), size, ppos
  , CameraTarget(..)
  , Velocity(..)
  , Friction(..)
  , BoundingBox(..)
  , Gravity(..)
  , PlayerInput(..)
  , PhysicsTime(..), time, accum
  , GlobalTime(..) )
import Game.Jump (landed, onGround, jumpRequested, jumping, floating)

-- constant acceleration
bumpX :: Unit -> System' ()
bumpX x = cmap $ \(Player, Acceleration (V2 _ y)) -> Acceleration $ V2 x y

setJump :: System' ()
setJump = cmapM_ $ \(Player, jumpState@(Jump _ _ _), e) -> do
  when (jumpState == onGround) $ set e jumpRequested

releaseJump :: System' ()
releaseJump = cmapM_ $ \(Player, jumpState@(Jump _ _ _), e) -> do
  when (jumpState == landed) $ set e onGround

handleEvent :: SDL.Event -> System' ()
handleEvent event = do
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      cmap $ \(PlayerInput m) ->
        if (member keyCode m)
        then PlayerInput $ insert keyCode motion m
        else PlayerInput m
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent
    _ -> return ()

type BoxEntity = (BoundingBox, Position, Entity)

hasVelComponent :: BoxEntity -> System' Bool
hasVelComponent (_, _, e) = do
  hasVelocity <- exists e (proxy :: Velocity)
  return hasVelocity

inNarrowPhase :: Entity -> AABB -> BoxEntity -> Bool
inNarrowPhase e sweptBox (BoundingBox bb, Position p, e') =
  (not $ e' == e) && (aabbCheck sweptBox $ AABB { center = p, dims = bb })

handleBaseCollision :: Entity -> Collision -> System' ()
handleBaseCollision e c@(Collision collisionTime normal _) = do
  ((Velocity v@(V2 vx vy)), Position p) <- get e :: System' (Velocity, Position)

  let cTime = dTinSeconds * collisionTime
      remainingTime = dTinSeconds * (1 - collisionTime)
      vNormal@(V2 normalX normalY) = toVector normal
      dotProd = ((vx * normalY) + (vy * normalX)) * Unit remainingTime
      v' =
        if (normal == TopN || normal == BottomN)
        then V2 vx (dotProd * normalX)
        else V2 (dotProd * normalY) vy
      p' = p + v' ^* Unit cTime

  set e (Velocity v', Position p')

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

handleJumpCheck :: Entity -> Collision -> System' ()
handleJumpCheck e (Collision _ normal _) = do
  hasJump <- exists e (proxy :: Jump)
  when (hasJump && normal == BottomN) $ do
    jumpState <- get e :: System' Jump
    when (jumpState == jumping) $ set e landed
    when (jumpState == floating) $ set e onGround

handleCollision :: Entity -> Collision -> System' ()
handleCollision e c = do
  handleBaseCollision e c
  handleFloor e c
  handleJumpCheck e c

type GetVelocity  = System' (Velocity)
type GetAABB      = System' (BoundingBox, Position)
type GetSweptAABB = System' (BoundingBox, Position, Velocity)

testCollision :: Entity -> BoxEntity -> System' ()
testCollision e (_, _, e') = do
  -- get current collision information
  (BoundingBox bb1, Position p1, v@(Velocity v')) <- get e  :: GetSweptAABB
  (BoundingBox bb2, Position p2)    <- get e' :: GetAABB
  let box1 = AABB { center = p1, dims = bb1 }
      box2 = AABB { center = p2, dims = bb2 }
      (collisionTime, normal) = sweepAABB v box1 box2
      collision = Collision collisionTime normal e'

  -- we hit a corner most likely
  when (normal == NoneN) $ do
    -- if we're touching a side of a box
    let p' = if (aabbCheck box1 box2)
             -- adjust position by penetration vector
             then p1 + (penetrationVector box1 box2)
             -- otherwise move normally (touching just a corner)
             else p1 + (v' ^* Unit dTinSeconds)
    set e (Position p')

  -- we have a normal collision
  unless (normal == NoneN) $ do
    handleCollision e collision

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
         -- update position based on time and velocity
         let p'' = p' + (v' ^* Unit dTinSeconds)
         set entity (Position p'')
     ) dynamics


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

runInputUpdates :: System' ()
runInputUpdates = do
  PlayerInput m <- get global

  case (m ! SDL.KeycodeW) of
      SDL.Pressed  -> setJump
      SDL.Released -> releaseJump

  case (m ! SDL.KeycodeA) of
      SDL.Pressed  -> bumpX (-playerSpeed)
      SDL.Released -> return ()

  case (m ! SDL.KeycodeD) of
      SDL.Pressed  -> bumpX playerSpeed
      SDL.Released -> return ()

runPhysics :: System' ()
runPhysics = do
  -- run updates based on input map
  runInputUpdates

  -- update acceleration based on gravity
  cmap $ \(Gravity, Velocity (V2 _ vy), Acceleration (V2 ax ay)) ->
    if vy > 0
    then Acceleration $ V2 ax (3 * initialJumpG)
    else Acceleration $ V2 ax initialJumpG

  -- jump!
  cmapM_ $ \(Player, jumpState@(Jump _ _ _), e) -> do
    Velocity (V2 vx _) <- get e :: System' (Velocity)
    when (jumpState == jumpRequested) $ do
      set e (Velocity $ V2 vx (-initialJumpVy), jumping)

  -- update velocity based on acceleration
  cmap $ \(Acceleration a, Velocity v) ->
    let v' = (v + (a ^* Unit dTinSeconds))
    in Velocity $ clampVelocity <$> v'

  -- collisions
  handleCollisions

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
