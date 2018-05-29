module Game.Types where

import qualified Data.Map as Map (Map)
import           Foreign.C.Types (CInt)
import           GHC.Int (Int32(..))
import           Linear (V2)
import           Apecs (Entity)
import qualified SDL (Texture, Keycode)
import           KeyState
import qualified Animate

import           Game.Constants (Seconds(..), Unit(..))
-- import           Game.Player (PlayerAction(..))

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

-- data Player =
--   Player PlayerAction
--   deriving Show

data Camera = Camera
  { size :: (V2 Unit)   -- camera height and width
  , ppos :: (V2 Unit) } -- past position for verlet transform
  deriving Show

data CameraTarget =
  CameraTarget Entity
  deriving Show

data Texture =
  Texture SDL.Texture (V2 CInt)


type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds

newtype SpriteSheet key =
  SpriteSheet (Animate.SpriteSheet key SDL.Texture Seconds)

data Gravity = Gravity

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

data Jump = Jump
  { buttonPressed :: Bool
  , isJumping     :: Bool
  , isGrounded    :: Bool }
  deriving (Eq, Show)
