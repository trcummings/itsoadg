{-# LANGUAGE FlexibleContexts #-}

module Game.System.Physics where

import qualified SDL
import           Control.Monad (when, unless)
import           Control.Monad.Extra (partitionM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Map ((!))
import           Data.Coerce (coerce)
import           Data.List (partition)
import           Linear (V2(..), V4(..), (^*), (*^), (^/))
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
import           Game.Util.Constants
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
  , Jump(..)
  , Position(..)
  , Velocity(..)
  , Gravity(..)
  , CollisionModule(..)
  , QueueEvent(..)
  , Audio'Command(..)
  , Player'SFX'Key(..) )

oneBumpPerSecond :: Unit
oneBumpPerSecond = (4 / 32) * Unit frameDeltaSeconds

stepPosition :: (Velocity, Position) -> Position
stepPosition (Velocity v, Position p) = Position $ p + (v ^* Unit frameDeltaSeconds)

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

stepJump :: (Jump, Velocity, Entity) -> (Velocity, [QueueEvent])
stepJump (jumpState@(Jump _ _), v@(Velocity (V2 vx _)), e) =
  if (requested jumpState && onGround jumpState)
  then
    ( Velocity $ V2 vx (-initialJumpVy)
    , [AudioSystemEvent (e, Player'SFX'Jump, Audio'PlayOrSustain)] )
  else (v, [])

updateNonCollidablePositions :: (Velocity, Position, Not CollisionModule) -> Position
updateNonCollidablePositions (v, p, _) = stepPosition (v, p)

stepPhysicsSystem :: [QueueEvent] -> System' [QueueEvent]
stepPhysicsSystem evts = do
  -- update acceleration based on gravity
  cmap handleGravity
  -- jump!
  evts' <- emap stepJump
  -- clamp velocity
  cmap handleVelocityClamp
  -- update positions of all non-colliding entities
  cmap updateNonCollidablePositions
  return (evts ++ evts')
