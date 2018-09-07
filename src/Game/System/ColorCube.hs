module Game.System.ColorCube where


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

import           Game.World.TH          (ECS)
import           Game.Util.Constants    (shaderPath)
import           Game.Types
  ( Degrees(..)
  , Orientation(..)
  , RotatingCube(..)
  , Position3D(..)
  , Model(..)
  , Resource(..)
  , ProjectionMatrix(..)
  , ViewMatrix(..) )

type ColorCube = (RotatingCube, Model, Position3D, Orientation)

initColorCube :: ECS ()
initColorCube = do
  let vertexShader   = shaderPath </> "cube.v.glsl"
      fragmentShader = shaderPath </> "cube.f.glsl"
      vertices       = [
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
      colors = [
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
  shaderProgram <- liftIO $ U.simpleShaderProgram vertexShader fragmentShader
  vertexBuffer  <- liftIO $ U.fromSource GL.ArrayBuffer vertices
  colorBuffer   <- liftIO $ U.fromSource GL.ArrayBuffer colors
  let resource = Resource { _shaderProgram = shaderProgram
                          , _vertexBuffer  = vertexBuffer
                          , _colorBuffer   = colorBuffer }
      model    = Model { _resource = resource
                       , _vertices = vertices
                       , _colors   = colors }

  -- cube 1
  newEntity (
      RotatingCube { _axis = L.V3 0 0 (-1)
                   , _deg  = Degrees 4 }
    , model
    , Position3D  $ L.V3 0 0 (-4)
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )

  -- cube 2
  newEntity (
      RotatingCube { _axis = L.V3 1 0 0
                   , _deg  = Degrees (-2) }
    , model
    , Position3D  $ L.V3 2 1 (-3)
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )
  return ()

-- rotate da cubes!!!
stepColorCube :: (RotatingCube, Orientation) -> Orientation
stepColorCube (rc, Orientation o) =
  let Degrees deg = _deg rc
  in  Orientation $ o * L.axisAngle (_axis rc) (U.deg2rad deg)

-- printGLErrors :: String -> IO ()
-- printGLErrors whereAt = do
--   errors <- GL.errors
--   liftIO $ putStrLn $ "at " ++ whereAt ++ ": " ++ show errors

drawColorCube :: (ProjectionMatrix, ViewMatrix) -> ColorCube -> IO ()
drawColorCube (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
              (_, model, Position3D mPos, Orientation o) = do
  let modelMatrix   = L.mkTransformation o mPos
      trans         = projMatrix
                  !*! viewMatrix
                  !*! modelMatrix
      vertexBuffer  = _vertexBuffer  . _resource $ model
      colorBuffer   = _colorBuffer   . _resource $ model
      shaderProgram = _shaderProgram . _resource $ model
      program       = U.program shaderProgram
      numVerts      = fromIntegral $ length $ _vertices model
      -- attribs & uniforms
      colLoc = U.getAttrib  shaderProgram "vertexColor"
      posLoc = U.getAttrib  shaderProgram "vertexPosition_modelspace"
      mvpLoc = U.getUniform shaderProgram "MVP"
  -- set current program to shaderProgram
  GL.currentProgram             $= Just program
  -- enable attribs
  GL.vertexAttribArray   posLoc $= GL.Enabled
  GL.vertexAttribArray   colLoc $= GL.Enabled
  -- transform mvp uniform
  trans `U.asUniform` mvpLoc
  -- bind to vertex buffer VB
  GL.bindBuffer GL.ArrayBuffer  $= Just vertexBuffer
  GL.vertexAttribPointer posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- bind to color buffer VB
  GL.bindBuffer GL.ArrayBuffer  $= Just colorBuffer
  GL.vertexAttribPointer colLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- draw vertex arrays
  GL.drawArrays GL.Triangles 0 numVerts
  -- disable all attributes
  GL.vertexAttribArray   posLoc $= GL.Disabled
  GL.vertexAttribArray   colLoc $= GL.Disabled
  -- unset current program
  GL.currentProgram             $= Nothing
