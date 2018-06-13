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
  , Not
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
import           Game.Wrapper.Apecs (emap)
import           Game.Types
  ( Unit(..)
  , Jump(..), buttonPressed, isJumping, isGrounded
  , Position(..)
  , Velocity(..)
  , Gravity(..)
  , CollisionModule(..)
  , QueueEvent(..)
  , Audio'Command(..)
  , Player'SFX'Key(..) )
import Game.Jump
  ( landed
  , onGround
  , jumpRequested
  , jumping
  , floating
  , falling )

oneBumpPerSecond :: Unit
oneBumpPerSecond = (4 / 32) * Unit frameDeltaSeconds

stepPosition :: (Velocity, Position) -> Position
stepPosition (Velocity v, Position p) = Position $ p + (v ^* Unit frameDeltaSeconds)

handleNotOnGround :: Entity -> System' ()
handleNotOnGround entity = do
  -- we are not on the ground if we arent colliding with anything, but we arent
  -- necessarily jumping
  hasJump <- exists entity (proxy :: Jump)
  when hasJump $ do
    jumpState <- get entity :: System' Jump
    when (jumpState == onGround || jumpState == landed) $ set entity falling

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

stepJump :: (Jump, Velocity, Entity) -> ((Jump, Velocity), [QueueEvent])
stepJump (jumpState@(Jump _ _ _), v@(Velocity (V2 vx _)), e) =
  if (jumpState == jumpRequested)
  then (
    ( jumping, Velocity $ V2 vx (-initialJumpVy))
    , [AudioSystemEvent (e, Player'SFX'Jump, Audio'PlayOrSustain)] )
  else ((jumpState, v), [])

updateNonCollidablePositions :: (Not CollisionModule, Velocity, Position) -> Position
updateNonCollidablePositions (_, v, p) = stepPosition (v, p)

stepPhysicsSystem :: [QueueEvent] -> System' [QueueEvent]
stepPhysicsSystem evts = do
  -- update acceleration based on gravity
  cmap handleGravity
  -- jump!
  -- cmap handleJumpRequest
  evts' <- emap stepJump
  -- clamp velocity
  cmap handleVelocityClamp
  -- update positions of all non-colliding entities
  cmap updateNonCollidablePositions

  return (evts ++ evts')
