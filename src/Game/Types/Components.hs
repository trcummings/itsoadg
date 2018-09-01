module Game.Types.Components where

import qualified SDL (Texture, Keycode)
import qualified SDL.Mixer as Mixer (Chunk, Channel)
import qualified Data.Map as Map (Map)
import qualified Animate
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as U
import qualified Linear as L
import           Foreign.C.Types (CInt, CDouble)
import           GHC.Int (Int32(..))
import           Linear (V2, V3)
import           Apecs (Entity)
import           KeyState

import           Game.Types.Util (Seconds(..), Unit(..), Step(..))
import           Game.Types.Physics (CollisionLayer, AABB, RaycastHit)
import           Game.Types.TileMap (TileType)
import           Game.Types.Audio (Audio'Command)
import           Game.Types.Player
  ( PlayerAction(..)
  , PlayerKey(..)
  , Player'SFX'Key(..) )

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

-- data Camera = Camera
--   { size :: (V2 Unit)   -- camera height and width
--   , ppos :: (V2 Unit) } -- past position for verlet transform
--   deriving Show

data Position3D = Position3D (V3 Float)

-- data Velocity3D = Velocity3D (V3 Unit)

-- Camera Types
data ClippingPlanes = ClippingPlanes { near :: Float
                                     , far  :: Float }

newtype FieldOfView = FieldOfView Float

data CameraAxes = CameraAxes { xAxis :: L.V3 Float
                             , yAxis :: L.V3 Float
                             , zAxis :: L.V3 Float }

newtype Orientation = Orientation (L.Quaternion Float)

data Camera = Camera { clippingPlanes :: ClippingPlanes
                     , fieldOfView    :: FieldOfView
                     , orientation    :: Orientation
                     , cameraAxes     :: CameraAxes }

-- OpenGL types
data Resource = Resource { shaderProgram :: U.ShaderProgram
                         , vertBuffer    :: GL.BufferObject
                         , colorBuffer   :: GL.BufferObject
                         , elementBuffer :: GL.BufferObject }

data Model = Model { resource  :: Resource
                   , vertices  :: [L.V3 Float]
                   , colors    :: [L.V3 Float]
                   , elements  :: [L.V3 GL.GLuint] }

-- data CameraTarget =
--   CameraTarget Entity
--   deriving Show
--
-- data Texture =
--   Texture SDL.Texture (V2 CInt)
--
-- -- type AnimationKey =
-- --   PlayerKey
--
-- -- type Animations key =
-- --   Animate.Animations key (Animate.SpriteClip key) Seconds
--
-- data SpriteSheet = SpriteSheet
--     (Animate.SpriteSheet AnimationKey SDL.Texture Seconds)
--     (Animate.Position AnimationKey Seconds)
--
-- data Gravity = Gravity
--   { ascent  :: Unit
--   , descent :: Unit }
--   deriving Show
--
-- newtype Friction =
--   Friction Double
--   deriving Show
--
-- data Font = Font [(Char, Texture)]

-- global input for player
type PlayerInputMap = (Map.Map SDL.Keycode (KeyState Double))
type NewlyModifiedInputs = (Map.Map SDL.Keycode Bool)
data PlayerInput = PlayerInput
  { inputs       :: PlayerInputMap
  , justModified :: NewlyModifiedInputs }
  deriving Show

data MousePosition =
  MousePosition (V2 Int32)

-- data Jump = Jump
--   { requested :: Bool
--   , onGround  :: Bool }
--   deriving (Eq, Show)
--
-- data FlowMeter = FlowMeter
--   { currentFlow :: Double
--   , baseFlow    :: Double
--   , flowLimit   :: Double
--   , counter     :: Double }
--   deriving Show
--
-- data HardFlow = HardFlow
--
-- -- For flow effect emitter state
-- data FlowEffectEmitState =
--     BurningFlow
--   | AbsorbingFlow
--   | NotEmittingFlowEffect
--   deriving Show
--
-- newtype FlowEffectEmitter =
--   FlowEffectEmitter FlowEffectEmitState
--   deriving Show
--
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

data Option =
  Option { oId      :: String
         , text     :: String
         , selected :: Bool   }

data OptionList = OptionList [Option]
