module Game.System.Scratch.BoundingBoxes where


import qualified Graphics.GLUtil           as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           SDL                    (($=))
import           Control.Monad          (mapM_)
import           Control.Monad.IO.Class (liftIO)
import           System.FilePath        ((</>))
import           Data.Map               ((!))
import           Control.Applicative
import           Apecs

import           Game.World.TH            (ECS)
import           Game.Util.Move           (Moveable)
import           Game.Util.Constants      (shaderPath)
import           Game.Util.BufferObjects  (fromSource)
import           Game.Loaders.Program     (createProgram, getUniform, getAttrib)
import           Game.Types
  ( Orientation(..)
  , Position3D(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , ShaderProgram(..)
  , SimpleCube(..)
  , ProjectionMatrix(..)
  , ViewMatrix(..)
  , CollisionModule(..)
  , ProgramMap(..)
  , ProgramName(..)
  )

type CamInfo = (ProjectionMatrix, ViewMatrix, Position3D)
data RenderGlobals = RenderGlobals
  { _rgCamera     :: CamInfo
  , _rgProgramMap :: ProgramMap }

_cPos  :: CamInfo -> Position3D
_cPos  (_, _, x) = x
_cPMat :: CamInfo -> ProjectionMatrix
_cPMat (x, _, _) = x
_cVMat :: CamInfo -> ViewMatrix
_cVMat (_, x, _) = x

makeProgram :: ProgramName -> IO ShaderProgram
makeProgram (ProgramName name) = do
  let vertexShader   = shaderPath </> (name ++ ".v.glsl")
      fragmentShader = shaderPath </> (name ++ ".f.glsl")
  createProgram [ ShaderInfo GL.VertexShader   vertexShader
                , ShaderInfo GL.FragmentShader fragmentShader ]

bbProgramName :: ProgramName
bbProgramName = ProgramName "simple_cube"

initBoundingBox :: IO ()
initBoundingBox = do
  return ()
  -- let vertexShader   = shaderPath </> "simple_cube.v.glsl"
  --     fragmentShader = shaderPath </> "simple_cube.f.glsl"
  -- vertexBuffer  <- fromSource (GL.StaticDraw, GL.ArrayBuffer) vs
  -- program       <- createProgram
  --                 [ ShaderInfo GL.VertexShader   vertexShader
  --                 , ShaderInfo GL.FragmentShader fragmentShader ]
  -- return
  --   ( BufferResource { _vertexBuffer   = Just vertexBuffer
  --                    , _texCoordBuffer = Nothing
  --                    , _normalBuffer   = Nothing
  --                    , _rgbCoordBuffer = Nothing
  --                    , _indexBuffer    = Nothing  }
  --   , program
  --   )


drawBoundingBox :: RenderGlobals
                -> (CollisionModule, Position3D)
                -> IO ()
drawBoundingBox globals (cm, Position3D mPos) = do
  let viewMatrix = (_viewMatrix . _cVMat . _rgCamera) globals
      projMatrix = (_projMatrix . _cPMat . _rgCamera) globals
      sProgram   = (_programMap . _rgProgramMap ) globals ! bbProgramName
  return ()
