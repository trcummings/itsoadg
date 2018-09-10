module Game.System.Scratch.TextureCube where

import qualified Graphics.GLUtil           as U
import qualified Graphics.GLUtil.Camera3D  as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           Linear                  ((!*!))
import           SDL                     (($=))
import           Control.Monad           (mapM_)
import           System.FilePath         ((</>))
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map                (keys)
import           Apecs                   (newEntity)

import           Game.World.TH            (ECS)
import           Game.Util.Constants      (objPath, texturePath, shaderPath)
import           Game.Loaders.Obj.Loader  (loadObjFile)
import           Game.Util.Shader.Program (createProgram)
import           Game.Util.Texture        (getAndCreateTexture)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ObjData(..)
  , TexResource(..)
  , RotatingCube(..)
  , ShaderInfo(..)
  , Degrees(..) )

type TexCube = (TexResource, RotatingCube, Orientation, Position3D)

initTextureCube :: ECS ()
initTextureCube = do
  let vertexShader   = shaderPath  </> "texcube.v.glsl"
      fragmentShader = shaderPath  </> "texcube.f.glsl"
      uvTexPath      = texturePath </> "texcube.tga"
      cubeObjPath    = objPath     </> "cube.obj"
  -- load in shaders
  program <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]
  -- load the obj file
  obj <- liftIO $ loadObjFile cubeObjPath
  -- load the image
  uv  <- liftIO $ getAndCreateTexture uvTexPath
  -- create the buffer related data
  vb  <- liftIO $ U.fromSource GL.ArrayBuffer $ _verts     obj
  uvb <- liftIO $ U.fromSource GL.ArrayBuffer $ _texCoords obj
  -- define the entity
  newEntity (
      TexResource { _sProgram   = program
                  , _texObj     = uv
                  , _vertBuffer = vb
                  , _uvBuffer   = uvb
                  , _objData    = obj }
    , RotatingCube { _axis = L.V3 1 0 (-1)
                   , _deg  = Degrees 0.5 }
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0)
    , Position3D $ L.V3 1 0 (-2) )
  return ()

drawTextureCube :: (ProjectionMatrix, ViewMatrix) -> TexCube -> IO ()
drawTextureCube (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
                (tr, _, Orientation o, Position3D cPos) = do
  let modelMatrix   = L.mkTransformation o cPos
      trans         = projMatrix
                  !*! viewMatrix
                  !*! modelMatrix
      shaderProgram = _sProgram   tr
      texture       = _texObj     tr
      vertices      = _vertBuffer tr
      uvCoords      = _uvBuffer   tr
      numVerts      = fromIntegral $ length $ _verts . _objData $ tr
      program       = U.program shaderProgram
      posLoc        = U.getAttrib  shaderProgram "vertexPosition_modelspace"
      uvvLoc        = U.getAttrib  shaderProgram "vertexUV"
      mtsLoc        = U.getUniform shaderProgram "myTextureSampler"
      mvpLoc        = U.getUniform shaderProgram "MVP"
  -- set current program to shaderProgram
  GL.currentProgram              $= Just program
  -- enable all attributes
  GL.vertexAttribArray    posLoc $= GL.Enabled
  GL.vertexAttribArray    uvvLoc $= GL.Enabled
  -- transform MVP uniform
  trans `U.asUniform` mvpLoc
  -- bind our texture to TextureUnit 0
  -- set "myTextureSampler" sampler to use Texture Unit 0
  GL.activeTexture               $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= texture
  GL.uniform mtsLoc              $= GL.Index1 (0 :: GL.GLint)
  -- bind position VB
  GL.bindBuffer GL.ArrayBuffer   $= Just vertices
  GL.vertexAttribPointer  posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- bind uv coords VB
  GL.bindBuffer GL.ArrayBuffer   $= Just uvCoords
  GL.vertexAttribPointer  uvvLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0 )
  -- draw indexed triangles
  GL.drawArrays GL.Triangles 0 numVerts
  -- disable all attributes
  GL.vertexAttribArray    posLoc $= GL.Disabled
  GL.vertexAttribArray    uvvLoc $= GL.Disabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer  $= Nothing
  -- unset current program
  GL.currentProgram             $= Nothing
