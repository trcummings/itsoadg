module Game.Types where

import           Foreign.C.Types (CInt)
import           Linear (V2)
import           Apecs (Entity)
import qualified SDL (Texture)

import           Game.Constants (Unit(..))

newtype Position =
  Position (V2 Unit)
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
  Player
  deriving Show

data Camera =
  Camera
  deriving Show

data Floor =
  Floor
  deriving Show

data Texture =
  Texture SDL.Texture (V2 CInt)

data Gravity = Gravity

newtype Friction =
  Friction Double
  deriving Show

data Font = Font [(Char, Texture)]

data Collisions =
  Collisions [Entity]
  deriving Show

-- accumulator for physics frame time updates
data PhysicsTime = PhysicsTime
  { time  :: Double
  , accum :: Double }
  deriving Show

-- global timer
newtype GlobalTime =
  GlobalTime Double
  deriving Show
