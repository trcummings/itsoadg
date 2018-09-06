module Game.System.ColorCube where


import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Linear ((!*!))
import           SDL (($=))
import           Apecs
import           Control.Monad (mapM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Applicative
import           System.FilePath ((</>))
import           Data.Map (keys)

import           Game.World.TH        (ECS)
import           Game.Util.Constants  (shaderPath)
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
      vertices       = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]
      colors         = vertices
      elements       = [ L.V3 2 1 0 -- right
                       , L.V3 1 2 3
                       , L.V3 0 1 4 -- top
                       , L.V3 4 1 5
                       , L.V3 4 5 6 -- left
                       , L.V3 7 6 5
                       , L.V3 2 6 3 -- bottom
                       , L.V3 6 3 7
                       , L.V3 0 4 2 -- front
                       , L.V3 2 4 6
                       , L.V3 5 1 7 -- back
                       , L.V3 7 1 3
                       ]
  shaderProgram <- liftIO $ U.simpleShaderProgram vertexShader fragmentShader
  vertexBuffer  <- liftIO $ U.fromSource GL.ArrayBuffer vertices
  colorBuffer   <- liftIO $ U.fromSource GL.ArrayBuffer colors
  elementBuffer <- liftIO $ U.fromSource GL.ElementArrayBuffer elements
  let resource = Resource { _shaderProgram = shaderProgram
                          , _vertexBuffer  = vertexBuffer
                          , _colorBuffer   = colorBuffer
                          , _elementBuffer = elementBuffer }
      model = Model { _resource = resource
                    , _vertices = vertices
                    , _colors   = colors
                    , _elements = elements }

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

drawColorCube :: (ProjectionMatrix, ViewMatrix) -> ColorCube -> IO ()
drawColorCube (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
         (_, model, Position3D mPos, Orientation o) = do
  let modelMatrix   = L.mkTransformation o mPos
      trans         = projMatrix
                  !*! viewMatrix
                  !*! modelMatrix
      resource      = _resource model
      vertices      = _vertices model
      colors        = _colors   model
      elements      = _elements model
      shaderProgram = _shaderProgram resource
      attribKeys    = keys $ U.attribs  shaderProgram
      uniformKeys   = keys $ U.uniforms shaderProgram
  -- set current program to shaderProgram
  GL.currentProgram $= (Just $ U.program shaderProgram)
  -- enable all attributes
  mapM_ (U.enableAttrib shaderProgram) attribKeys
  -- bind all buffers & set all attributes
  GL.bindBuffer GL.ArrayBuffer $= Just (_vertexBuffer resource)

  U.setAttrib
    shaderProgram
    "coord3d"
    GL.ToFloat $
    GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  GL.bindBuffer GL.ArrayBuffer $= Just (_colorBuffer resource)

  U.setAttrib
    shaderProgram
    "v_color"
    GL.ToFloat $
    GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  -- transform all uniforms
  mapM_ (\keys -> do
        U.asUniform $ trans
      $ U.getUniform shaderProgram keys
    ) uniformKeys

  -- bind element buffer
  GL.bindBuffer GL.ElementArrayBuffer $= Just (_elementBuffer resource)

  -- draw indexed triangles
  U.drawIndexedTris (fromIntegral $ length elements)

  -- disable all attributes
  mapM_ (\key -> do
      GL.vertexAttribArray (U.getAttrib shaderProgram key) $= GL.Disabled
    ) attribKeys
