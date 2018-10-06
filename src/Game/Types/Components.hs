module Game.Types.Components where

-- import qualified SDL (Texture, Keycode)
-- import qualified SDL.Mixer as Mixer (Chunk, Channel)
-- import qualified Data.Map as Map (Map)
import qualified Animate
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Apecs (Entity)
import           Data.Map

import           Game.Types.Loaders.Obj (ObjData)
import           Game.Types.Shader      (ShaderProgram)
-- import           Foreign.C.Types (CInt, CDouble)
-- import           GHC.Int (Int32(..))
-- import           Linear (V2, V3)
-- import           Apecs (Entity)
-- import           KeyState

-- import           Game.Types.Util      (Seconds(..))
-- import           Game.Types.Animation (AnimationKey(..))
-- import           Game.Types.Physics (CollisionLayer, AABB, RaycastHit)
-- import           Game.Types.TileMap (TileType)
-- import           Game.Types.Audio (Audio'Command)
-- import           Game.Types.Player
--   ( PlayerAction(..)
--   , PlayerKey(..)
--   , Player'SFX'Key(..) )

-- -- Component types
-- newtype Position =
--   Position (V2 Unit) -- center point
--   deriving Show
--
-- newtype Velocity =
--   Velocity (V2 Unit)
--   deriving Show
--
-- newtype Acceleration =
--   Acceleration (V2 Unit)
--   deriving Show
--
-- newtype BoundingBox =
--   BoundingBox (V2 Unit)
--   deriving Show
--
-- data Player =
--   Player (Step PlayerAction)
--   deriving Show

data Hierarchy = Hierarchy
  { _parent   :: Maybe Entity
  , _children :: Maybe [Entity] }
  deriving Show

data PolygonModeType =
    PolygonMode'Wireframe
  | PolygonMode'Normal

newtype PolygonMode = PolygonMode PolygonModeType

instance Monoid PolygonMode where
  mempty  = PolygonMode PolygonMode'Normal
  mappend (PolygonMode p1) (PolygonMode p2) = PolygonMode p2


newtype Position3D =
  Position3D (L.V3 Float)
  deriving Show

newtype Orientation =
  Orientation (L.Quaternion Float)
  deriving Show

-- Option Menu Types
data OptionMenuCommand =
    MoveUp
  | MoveDown
  | SelectOption
  deriving Eq

newtype HasOptionMenuEvent =
  HasOptionMenuEvent OptionMenuCommand

data ActiveOptionList = ActiveOptionList

data Option = Option
  { _oId      :: String
  , _text     :: String
  , _selected :: Bool   }

data OptionList = OptionList [Option]

-- Movement types
newtype Degrees = Degrees Float deriving Show

newtype Translation =
  Translation (L.V3 Float)
  deriving Show

data Rotation =
    Yaw
  | Pitch
  | Roll
  deriving Show

data MoveCommand =
    Move'Translate Translation
  | Move'Rotate    Rotation Degrees
  | Move'Compose   MoveCommand MoveCommand
  deriving Show

newtype HasMoveCommand = HasMoveCommand MoveCommand


-- Camera Types
data ClippingPlanes = ClippingPlanes
  { _near :: Float
  , _far  :: Float }

newtype FieldOfView = FieldOfView Float

data CameraAxes = CameraAxes
  { _xAxis :: L.V3 Float
  , _yAxis :: L.V3 Float
  , _zAxis :: L.V3 Float }

data Camera = Camera
  { _clippingPlanes :: ClippingPlanes
  , _fieldOfView    :: FieldOfView
  , _cameraAxes     :: CameraAxes }

-- OpenGL types
newtype VAO = VAO GL.VertexArrayObject

data BufferResource = BufferResource
  { _vertexBuffer   :: Maybe GL.BufferObject
  , _texCoordBuffer :: Maybe GL.BufferObject
  , _normalBuffer   :: Maybe GL.BufferObject
  , _rgbCoordBuffer :: Maybe GL.BufferObject
  , _indexBuffer    :: Maybe GL.BufferObject }

data Texture = Texture
  { _textureId   :: Maybe GL.TextureObject
  , _textureSize :: GL.TextureSize2D }
  deriving Show

newtype ProgramName = ProgramName String deriving (Show, Eq, Ord)
newtype Renderable  = Renderable ProgramName
newtype ProgramMap  = ProgramMap
  { _programMap :: Map ProgramName (BufferResource, ShaderProgram) }

instance Monoid ProgramMap where
  mempty  = ProgramMap empty
  mappend = mappend



newtype ProjectionMatrix = ProjectionMatrix { _projMatrix :: (L.M44 Float) }
newtype ViewMatrix       = ViewMatrix       { _viewMatrix :: (L.M44 Float) }

newtype Terrain = Terrain TerrainConfig

data TerrainConfig = TerrainConfig
  { _trSize      :: Float
  , _trVertCount :: Float }

data TerrainInfo = TerrainInfo
  { _trVertices  :: [L.V3 Float]
  , _trTexCoords :: [L.V2 Float]
  , _trNormals   :: [L.V3 Float]
  , _trIndices   :: [L.V3 Int] }


data Billboard = Billboard

newtype FloorCircle = FloorCircle Float

data Door = Door deriving Show

data DoorPanel =
    DoorPanel'Top
  | DoorPanel'Bottom
  deriving Show


data RotatingCube = RotatingCube
  { _axis :: (L.V3 Float)
  , _deg  :: Degrees }
  deriving Show


data SimpleCube = SimpleCube

newtype Player = Player Facing deriving Show

data CardinalDir =
    CardinalDir'North -- (+Z axis)
  | CardinalDir'South -- (-Z axis)
  | CardinalDir'East  -- (+X axis)
  | CardinalDir'West  -- (-X axis)

instance Show CardinalDir where
  show CardinalDir'North = "North"
  show CardinalDir'South = "South"
  show CardinalDir'East  = "East"
  show CardinalDir'West  = "West"

newtype Facing = Facing CardinalDir deriving Show


newtype Frustum = Frustum Bool

-- data CameraTarget =
--   CameraTarget Entity
--   deriving Show
--
--
-- data Gravity = Gravity
--   { ascent  :: Unit
--   , descent :: Unit }
--   deriving Show
--
-- data Jump = Jump
--   { requested :: Bool
--   , onGround  :: Bool }
--   deriving (Eq, Show)
-- type SFX'Key =
--     Player'SFX'Key
--
-- data SoundBank = SoundBank
--   { bank       :: (Map.Map SFX'Key Mixer.Chunk)
--   , channelMap :: (Map.Map Entity (SFX'Key, Mixer.Channel)) }
--   deriving Show

data Collider =
  BoxCollider (L.V3 Float)
  deriving Show

data CollisionModule = CollisionModule
  { _collider     :: Collider
  , _hasCollision :: Bool }
  deriving Show
-- data CollisionModule = CollisionModule
--  { layer :: CollisionLayer
--  , layerCollisions :: [( CollisionLayer
--                        , RaycastHit
--                        , Either Entity TileType )] }
--  deriving Show
--
-- data Commandable = Commandable

data SanityMeter = SanityMeter
  { _currentSanity :: Float
  , _maxSanity     :: Float }

instance Monoid SanityMeter where
  mempty = SanityMeter 100 100
  mappend (SanityMeter cs1 ms1) (SanityMeter cs2 ms2) =
    SanityMeter { _currentSanity = cs1 + cs2
                , _maxSanity     = ms1 + ms2 }

data StaminaMeter = StaminaMeter
  { _currentStamina :: Float
  , _maxStamina     :: Float }

instance Monoid StaminaMeter where
  mempty = StaminaMeter 100 100
  mappend (StaminaMeter cs1 ms1) (StaminaMeter cs2 ms2) =
    StaminaMeter { _currentStamina = cs1 + cs2
                 , _maxStamina     = ms1 + ms2 }
