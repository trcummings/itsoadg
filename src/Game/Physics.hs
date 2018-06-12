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
import           Game.Collision (handleCollisions)
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
import           Game.Types
  ( Unit(..)
  , Jump(..), buttonPressed, isJumping, isGrounded
  , Position(..)
  , Velocity(..)
  , Gravity(..) )
import Game.Jump
  ( landed
  , onGround
  , jumpRequested
  , jumping
  , floating
  , falling )

oneBumpPerSecond :: Unit
oneBumpPerSecond = (4 / 32) * Unit frameDeltaSeconds

stepPosition :: (Velocity, Position) -> (Velocity, Position)
stepPosition (v@(Velocity v'), Position p) =
  (v, Position $ p + (v' ^* Unit frameDeltaSeconds))

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

stepPhysics :: System' ()
stepPhysics = do
  -- update acceleration based on gravity
  cmap handleGravity

  -- jump!
  cmap handleJumpRequest

  -- clamp velocity
  cmap handleVelocityClamp

  -- collisions
  -- position will only be modified in here (as well as other things)
  handleCollisions
