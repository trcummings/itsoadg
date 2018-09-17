module Game.System.Scratch.ColorCube where


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
import           Game.Util.Constants      (shaderPath)
import           Game.Util.BufferObjects (fromSource)
import           Game.Loaders.Program (createProgram, getUniform, getAttrib)
import           Game.Types
  ( Degrees(..)
  , Orientation(..)
  , RotatingCube(..)
  , Position3D(..)
  -- , Model(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , ShaderProgram(..)
  -- , Resource(..)
  , Player(..)
  , ProjectionMatrix(..)
  , ViewMatrix(..) )

type Cube a = (a, BufferResource, ShaderProgram, Position3D, Orientation)
type PlayerCube = Cube Player
type ColorCube = Cube RotatingCube

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

cs :: [L.V3 Float]
cs = [
    L.V3 0.583 0.771 0.014
  , L.V3 0.609 0.115 0.436
  , L.V3 0.327 0.483 0.844
  , L.V3 0.822 0.569 0.201
  , L.V3 0.435 0.602 0.223
  , L.V3 0.310 0.747 0.185
  , L.V3 0.597 0.770 0.761
  , L.V3 0.559 0.436 0.730
  , L.V3 0.359 0.583 0.152
  , L.V3 0.483 0.596 0.789
  , L.V3 0.559 0.861 0.639
  , L.V3 0.195 0.548 0.859
  , L.V3 0.014 0.184 0.576
  , L.V3 0.771 0.328 0.970
  , L.V3 0.406 0.615 0.116
  , L.V3 0.676 0.977 0.133
  , L.V3 0.971 0.572 0.833
  , L.V3 0.140 0.616 0.489
  , L.V3 0.997 0.513 0.064
  , L.V3 0.945 0.719 0.592
  , L.V3 0.543 0.021 0.978
  , L.V3 0.279 0.317 0.505
  , L.V3 0.167 0.620 0.077
  , L.V3 0.347 0.857 0.137
  , L.V3 0.055 0.953 0.042
  , L.V3 0.714 0.505 0.345
  , L.V3 0.783 0.290 0.734
  , L.V3 0.722 0.645 0.174
  , L.V3 0.302 0.455 0.848
  , L.V3 0.225 0.587 0.040
  , L.V3 0.517 0.713 0.338
  , L.V3 0.053 0.959 0.120
  , L.V3 0.393 0.621 0.362
  , L.V3 0.673 0.211 0.457
  , L.V3 0.820 0.883 0.371
  , L.V3 0.982 0.099 0.879
  ]

initColorCube :: ECS ()
initColorCube = do
  let vertexShader   = shaderPath </> "cube.v.glsl"
      fragmentShader = shaderPath </> "cube.f.glsl"
  vertexBuffer  <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) vs
  colorBuffer   <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) cs
  program       <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]

  -- cube 1
  newEntity (
      Player
    , BufferResource { _vertexBuffer   = Just vertexBuffer
                     , _texCoordBuffer = Nothing
                     , _normalBuffer   = Nothing
                     , _rgbCoordBuffer = Just colorBuffer
                     , _indexBuffer    = Nothing  }
    , program
    , Position3D  $ L.V3 0 0 (-3)
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )

  -- -- cube 2
  -- newEntity (
  --     RotatingCube { _axis = L.V3 1 0 0
  --                  , _deg  = Degrees (-2) }
  --   , model
  --   , program
  --   , Position3D  $ L.V3 3 1 (-3)
  --   , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )
  return ()

-- rotate da cubes!!!
stepColorCube :: (RotatingCube, Orientation) -> Orientation
stepColorCube (rc, Orientation o) =
  let Degrees deg = _deg rc
  in  Orientation $ o * L.axisAngle (_axis rc) (U.deg2rad deg)


drawColorCube :: (ProjectionMatrix, ViewMatrix) -> Cube a -> IO ()
drawColorCube (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
              (_, br, shaderProgram, Position3D mPos, Orientation o) = do
  let modelMatrix   = L.mkTransformation o mPos
      trans         = projMatrix
                  !*! viewMatrix
                  !*! modelMatrix
      vertexBuffer  = _vertexBuffer   br
      colorBuffer   = _rgbCoordBuffer br
      program       = _glProgram shaderProgram
      -- attribs & uniforms
      colLoc = getAttrib  shaderProgram "vertexColor"
      posLoc = getAttrib  shaderProgram "vertexPosition_modelspace"
  -- set current program to shaderProgram
  GL.currentProgram             $= Just program
  -- enable attribs
  GL.vertexAttribArray   posLoc $= GL.Enabled
  GL.vertexAttribArray   colLoc $= GL.Enabled
  -- transform mvp uniform
  trans `U.asUniform` (getUniform shaderProgram "MVP")
  -- bind to vertex buffer VB
  GL.bindBuffer GL.ArrayBuffer  $= vertexBuffer
  GL.vertexAttribPointer posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- bind to color buffer VB
  GL.bindBuffer GL.ArrayBuffer  $= colorBuffer
  GL.vertexAttribPointer colLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- draw vertex arrays
  GL.drawArrays GL.Triangles 0 (fromIntegral $ length vs)
  -- disable all attributes
  GL.vertexAttribArray   posLoc $= GL.Disabled
  GL.vertexAttribArray   colLoc $= GL.Disabled
  -- unset current program
  GL.currentProgram             $= Nothing
