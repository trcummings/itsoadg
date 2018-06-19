module Game.Types.Physics where

import           Linear (V2(..))

import           Game.Types.Util (Unit(..))

-- Physics Types
data AABB = AABB
  { center :: (V2 Unit)   -- x y pos
  , dims   :: (V2 Unit) } -- width and height
  deriving Show

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

data CollisionType =
    NoCollision
  | SweptCollision  (CollisionTime    , CollisionNormal)
  | SimpleCollision (PenetrationVector, CollisionNormal)
  deriving Show

data CollisionLayer =
    CL'Player
  | CL'Collectible
  | CL'Trigger
  | CL'Surface
  | CL'EmptyLayer

data RaycastHit = RaycastHit
 { distance :: Unit
 , fraction :: Unit
 , position :: V2 Unit
 , normal   :: V2 Unit }
 deriving Show

