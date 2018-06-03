module Game.Types.Physics where

import           Linear (V2(..))
import           Apecs (Entity(..))

import           Game.Types.Util (Unit(..))

-- Physics Types
data AABB = AABB
  { center :: (V2 Unit)   -- x y pos
  , dims   :: (V2 Unit) } -- width and height
  deriving Show

-- type BoxEntity = (BoundingBox, Position, Entity)

newtype PenetrationVector =
  PenetrationVector (V2 Unit)
  deriving (Show)

newtype CollisionTime =
  CollisionTime Double
  deriving (Show)

data CollisionNormal =
    LeftNormal
  | RightNormal
  | TopNormal
  | BottomNormal
  | NoneNormal -- the "zero normal"
  deriving (Eq, Show)

data Collision =
  Collision CollisionTime CollisionNormal PenetrationVector Entity
  deriving Show
