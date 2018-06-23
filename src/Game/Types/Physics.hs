module Game.Types.Physics where

import           Linear (V2(..))

import           Game.Types.Util (Unit(..))
import           Game.Types.TileMap (TileType)

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

data CollisionLayerType =
    CLT'Solid
  | CLT'NonSolid
  | CLT'NonCollidable
  deriving (Eq, Show)

data CollisionLayer =
    CL'Player
  | CL'Collectible
  | CL'Tile
  deriving (Eq, Show)

data Axis = X | Y deriving (Eq, Show)

data SensorDirection =
    Sensor'Top
  | Sensor'Left
  | Sensor'Right
  | Sensor'Bottom
  deriving Show

data Ray = Ray
  { origin    :: V2 Unit
  , delta     :: V2 Unit }
  deriving (Eq, Show)

data RaycastHit = RaycastHit
 { hitTime  :: Unit
 , distance :: V2 Unit
 , position :: V2 Unit
 , normal   :: V2 Unit }
 deriving (Eq, Show)

type CollisionEvent = (RaycastHit, (V2 Unit, V2 Unit))
