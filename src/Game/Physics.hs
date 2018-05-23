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
  , Not
  , cmap
  , cmapM_
  , set
  , get
  , getAll
  , proxy
  , exists
  , global )

import           Game.World (System')
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
  , Floor(..)
  , Friction(..)
  , BoundingBox(..)
  , Gravity(..)
  , Collisions(..)
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

toAABB :: V2 Unit -> V2 Unit -> V4 Unit
toAABB (V2 x y) (V2 w h) = V4 x y (x + w) (y + h)

aabbIntersection :: V4 Unit -> V4 Unit -> Bool
aabbIntersection (V4 tlx1 tly1 brx1 bry1) (V4 tlx2 tly2 brx2 bry2) =
  (brx1 >= tlx2) && (tlx1 <= brx2) && (bry1 >= tly2) && (tly1 <= bry2)

handleFloor :: Entity -> System' ()
handleFloor e = do
  (_, Friction f, Position p') <- get e :: System' (Floor, Friction, Position)

  cmap $ \(Player, Velocity (V2 vx _)) ->
    -- apply horizontal friction
    let vx' = (vx * Unit f)
    -- set vertical velocity to 0
        vy' = 0
    in Velocity $ V2 vx' vy'

  -- clear isJumping
  cmap $ \(Player, Jump _ _) ->
    Jump { jumpCommandReceived = False, isJumping = False }


type BoxEntity = (BoundingBox, Position, Entity)

hasVelComponent :: BoxEntity -> System' Bool
hasVelComponent (_, _, e) = do
  hasVelocity <- exists e (proxy :: Velocity)
  return hasVelocity

handleCollisions :: System' ()
handleCollisions = do
  -- get all entities with a bounding box
  allBoundingBoxes <- getAll :: System' [BoxEntity]

  -- partition into moving boxes & static boxes
  (dynamics, statics) <- partitionM hasVelComponent allBoundingBoxes

  -- O(n^2) algorithm, optimize later
  mapM (\(BoundingBox bb, Position p, entity) -> do
      let shouldAABB = aabbIntersection $ toAABB p bb
          actives = filter (\(BoundingBox bb', Position p', e') ->
            (not $ e' == entity) && (shouldAABB $ toAABB p' bb')) allBoundingBoxes
          entities = map (\(_, _, e) -> e) actives
      set entity (Collisions entities)
    ) allBoundingBoxes

  -- resolve collisions
  cmapM_ $ \(Player, Collisions es) -> do
    -- loop over each collision. if its a wall, if its a floor, else ignore
    mapM (\e -> do
     isFloor <- exists e (proxy :: (Floor, Position))
     when isFloor $ (handleFloor e) ) es

  -- clear remaining collisions
  cmap $ \(Collisions _) -> Collisions []



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

  -- update position based on time and velocity
  cmap $ \(Velocity v, Position p) -> Position $ p + (v ^* Unit dTinSeconds)

  -- update camera
  stepCamera

  -- clamp player position to screen edges
  cmap $ \(Player, Position (V2 x y)) -> Position $ V2
    (min (screenWidth  - 1) . max 0 $ x)
    (min (screenHeight - 1) . max 0 $ y)

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
