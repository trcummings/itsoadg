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

-- Aliases
-- type BoxEntity = (CollisionModule, BoundingBox, Position, Entity)
--
-- type AnimationKey =
--   PlayerKey
--
-- type Animations key =
--   Animate.Animations key (Animate.SpriteClip key) Seconds
--
--
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

data HierarchyCons a =
  HierarchyCons a [HierarchyCons a]

-- data Camera = Camera
--   { size :: (V2 Unit)   -- camera height and width
--   , ppos :: (V2 Unit) } -- past position for verlet transform
--   deriving Show

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

-- data Resource = Resource
--   { _shaderProgram :: ShaderProgram
--   , _vertexBuffer  :: GL.BufferObject
--   , _colorBuffer   :: GL.BufferObject }

-- data Model = Model
--   { _resource  :: Resource
--   , _vertices  :: [L.V3 Float]
--   , _colors    :: [L.V3 Float] }
--   -- , _elements  :: [L.V3 GL.GLuint] }

newtype ProjectionMatrix = ProjectionMatrix (L.M44 Float)
newtype ViewMatrix       = ViewMatrix       (L.M44 Float)


data Player    = Player
data Terrain   = Terrain
data Billboard = Billboard

data Door = Door deriving Show

data DoorPanel =
    DoorPanel'Top
  | DoorPanel'Bottom
  deriving Show

-- data TexResource = TexResource
--   { _sProgram   :: ShaderProgram
--   , _texObj     :: Maybe GL.TextureObject
--   , _vertBuffer :: GL.BufferObject
--   , _uvBuffer   :: GL.BufferObject
--   , _objData    :: ObjData }

-- data BBResource = BBResource
--   { _bbSProgram   :: ShaderProgram
--   , _bbTexObj     :: Maybe GL.TextureObject
--   , _bbVertBuffer :: GL.BufferObject }

data RotatingCube = RotatingCube
  { _axis :: (L.V3 Float)
  , _deg  :: Degrees }
  deriving Show

data BufferResource = BufferResource
  { _vertexBuffer   :: Maybe GL.BufferObject
  , _texCoordBuffer :: Maybe GL.BufferObject
  , _normalBuffer   :: Maybe GL.BufferObject
  , _rgbCoordBuffer :: Maybe GL.BufferObject
  , _indexBuffer    :: Maybe GL.BufferObject }

newtype Texture = Texture (Maybe GL.TextureObject) deriving Show


-- Font types
data DebugHUD = DebugHUD
 { _hudInfo :: HUDInfo
 , _fontMap :: FontMap }

data HUDType =
    FPSCounter
  | PositionTracker
  deriving (Eq, Ord)

newtype HUDInfo = HUDInfo (Map HUDType FontInfo)
newtype FontMap = FontMap (Map Char    Character)

data FontInfo = FontInfo
  { _fText :: String
  , _fxPos :: Float
  , _fyPos :: Float
  , _fSize :: Float }

data Character = Character
  -- texture object for character
  { _charTexture :: Texture
  -- size of glyph
  , _charSize    :: L.V2 Float
  -- offset from baseline to left/top of glyph
  , _charBearing :: L.V2 Float
  -- offset to advance to next glyph
  , _charAdvance :: Int }
  deriving Show


-- data CameraTarget =
--   CameraTarget Entity
--   deriving Show
--
-- data Texture =
--   Texture SDL.Texture (V2 CInt)
--
--
-- -- type Animations key =
-- --   Animate.Animations key (Animate.SpriteClip key) Seconds
--
-- data SpriteSheet = SpriteSheet
--   (Animate.SpriteSheet AnimationKey Texture Seconds)
--   (Animate.Position AnimationKey Seconds)
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
--
-- data CollisionModule = CollisionModule
--  { layer :: CollisionLayer
--  , layerCollisions :: [( CollisionLayer
--                        , RaycastHit
--                        , Either Entity TileType )] }
--  deriving Show
--
-- data Commandable = Commandable
