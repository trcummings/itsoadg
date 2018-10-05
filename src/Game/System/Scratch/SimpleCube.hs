module Game.System.Scratch.SimpleCube where


import qualified Graphics.GLUtil           as U
import qualified Graphics.GLUtil.Camera3D  as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           Linear                 ((!*!))
import           SDL                    (($=))
import           Control.Monad          (mapM_)
import           Control.Monad.IO.Class (liftIO)
import           System.FilePath        ((</>))
import           Data.Map               (keys)
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
  )

vs :: [L.V3 Float]
vs = [
    L.V3 (-1) (-1) (-1)
  , L.V3 (-1) (-1)  1
  , L.V3 (-1)   1   1
  , L.V3   1    1 (-1)
  , L.V3 (-1) (-1) (-1)
  , L.V3 (-1)   1  (-1)
  , L.V3   1  (-1)   1
  , L.V3 (-1) (-1) (-1)
  , L.V3   1  (-1) (-1)
  , L.V3   1    1  (-1)
  , L.V3   1  (-1) (-1)
  , L.V3 (-1) (-1) (-1)
  , L.V3 (-1) (-1) (-1)
  , L.V3 (-1)   1    1
  , L.V3 (-1)   1  (-1)
  , L.V3   1  (-1)   1
  , L.V3 (-1) (-1)   1
  , L.V3 (-1) (-1) (-1)
  , L.V3 (-1)   1    1
  , L.V3 (-1) (-1)   1
  , L.V3   1  (-1)   1
  , L.V3   1    1    1
  , L.V3   1  (-1) (-1)
  , L.V3   1    1  (-1)
  , L.V3   1  (-1) (-1)
  , L.V3   1    1    1
  , L.V3   1  (-1)   1
  , L.V3   1    1    1
  , L.V3   1    1  (-1)
  , L.V3 (-1)   1  (-1)
  , L.V3   1    1    1
  , L.V3 (-1)   1  (-1)
  , L.V3 (-1)   1    1
  , L.V3   1    1    1
  , L.V3 (-1)   1    1
  , L.V3   1  (-1)   1
  ]

type SimpleCubeProto = (SimpleCube, ProgramInfo, Moveable, CollisionModule)
type ProgramInfo = (BufferResource, ShaderProgram)

initCubeGLAttrs :: IO BufferResource
initCubeGLAttrs = do
  vertexBuffer <- fromSource (GL.StaticDraw, GL.ArrayBuffer) vs
  return $ BufferResource { _vertexBuffer   = Just vertexBuffer
                          , _texCoordBuffer = Nothing
                          , _normalBuffer   = Nothing
                          , _rgbCoordBuffer = Nothing
                          , _indexBuffer    = Nothing  }
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

drawSimpleCube :: (ProjectionMatrix, ViewMatrix) -> SimpleCubeProto -> IO ()
drawSimpleCube (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
               (_, (br, shaderProgram), (Orientation o, Position3D mPos), _) = do
  let modelMatrix   = L.mkTransformation o mPos
      vertexBuffer  = _vertexBuffer br
      program       = _glProgram shaderProgram
      -- attribs & uniforms
      posLoc = getAttrib  shaderProgram "VertexPosition_modelspace"
      mLoc   = getUniform shaderProgram "ModelMatrix"
      vLoc   = getUniform shaderProgram "ViewMatrix"
      pLoc   = getUniform shaderProgram "ProjMatrix"
      cLoc   = getUniform shaderProgram "BoxColor"
  -- set current program to shaderProgram
  GL.currentProgram             $= Just program
  -- enable attribs
  GL.vertexAttribArray   posLoc $= GL.Enabled
  -- transform mvp uniform
  modelMatrix `U.asUniform` mLoc
  viewMatrix  `U.asUniform` vLoc
  projMatrix  `U.asUniform` pLoc
  -- box color
  (L.V3 0.702 0.729 0.655 :: L.V3 Float) `U.asUniform` cLoc
  -- bind to vertex buffer VB
  GL.bindBuffer GL.ArrayBuffer  $= vertexBuffer
  GL.vertexAttribPointer posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- draw vertex arrays
  GL.drawArrays GL.Triangles 0 (fromIntegral $ length vs)
  -- disable all attributes
  GL.vertexAttribArray   posLoc $= GL.Disabled
  -- unset current program
  GL.currentProgram             $= Nothing
