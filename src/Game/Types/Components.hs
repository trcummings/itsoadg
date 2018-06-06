module Game.Types.Components where

import qualified SDL (Texture, Keycode)
import qualified SDL.Mixer as Mixer (Chunk)
import qualified Data.Map as Map (Map)
import qualified Animate
import           Foreign.C.Types (CInt, CDouble)
import           GHC.Int (Int32(..))
import           Linear (V2)
import           Apecs (Entity)
import           KeyState

import           Game.Types.Util (Seconds(..), Unit(..), Step(..))
import           Game.Types.Physics (Collision(..))
import           Game.Types.Player
  ( PlayerAction(..)
  , PlayerKey(..)
  , Player'SFX'Key(..) )

-- Aliases
type BoxEntity = (BoundingBox, Position, Entity)

type AnimationKey =
  PlayerKey

type Animations key =
  Animate.Animations key (Animate.SpriteClip key) Seconds


-- Component types
newtype Position =
  Position (V2 Unit) -- center point
  deriving Show

newtype Velocity =
  Velocity (V2 Unit)
  deriving Show

newtype Acceleration =
  Acceleration (V2 Unit)
  deriving Show

newtype BoundingBox =
  BoundingBox (V2 Unit)
  deriving Show

data Player =
  Player (Step PlayerAction)
  deriving Show

data Camera = Camera
  { size :: (V2 Unit)   -- camera height and width
  , ppos :: (V2 Unit) } -- past position for verlet transform
  deriving Show

data CameraTarget =
  CameraTarget Entity
  deriving Show

data Texture =
  Texture SDL.Texture (V2 CInt)

-- type AnimationKey =
--   PlayerKey

-- type Animations key =
--   Animate.Animations key (Animate.SpriteClip key) Seconds

data SpriteSheet = SpriteSheet
    (Animate.SpriteSheet AnimationKey SDL.Texture Seconds)
    (Animate.Position AnimationKey Seconds)

data Gravity = Gravity
  { ascent  :: Unit
  , descent :: Unit }
  deriving Show

newtype Friction =
  Friction Double
  deriving Show

data Font = Font [(Char, Texture)]

-- accumulator for physics frame time updates
data PhysicsTime = PhysicsTime
  { time  :: Double
  , accum :: Double }
  deriving Show

-- global timer
newtype GlobalTime =
  GlobalTime Double
  deriving Show

-- global input for player
data PlayerInput =
  PlayerInput (Map.Map SDL.Keycode (KeyState Double))
  deriving Show

data MousePosition =
  MousePosition (V2 Int32)

newtype Inbox = Inbox [Collision]

data Jump = Jump
  { buttonPressed :: Bool
  , isJumping     :: Bool
  , isGrounded    :: Bool }
  deriving (Eq, Show)

data FlowMeter = FlowMeter
  { currentFlow :: Double
  , baseFlow    :: Double
  , flowLimit   :: Double
  , counter     :: Double }
  deriving Show

data HardFlow = HardFlow

-- For flow effect emitter state
data FlowEffectEmitState =
    BurningFlow
  | AbsorbingFlow
  | NotEmittingFlowEffect
  deriving Show

newtype FlowEffectEmitter =
  FlowEffectEmitter FlowEffectEmitState
  deriving Show


type SFX'Key =
    Player'SFX'Key

data SoundBank = SoundBank
  { bank  :: (Map.Map SFX'Key Mixer.Chunk)
  , inbox :: [SFX'Key] }
  deriving Show