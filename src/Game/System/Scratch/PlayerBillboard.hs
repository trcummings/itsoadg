module Game.System.Scratch.PlayerBillboard where

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

import           Game.World.TH           (ECS)
import           Game.Util.Constants     (objPath, texturePath, shaderPath)
import           Game.Loaders.Obj.Loader (loadObjFile)
import           Game.Util.Texture       (getAndCreateTexture)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ObjData(..)
  , TexResource(..)
  , Player(..) )

type PlayerB = (Player, TexResource, Orientation, Position3D)

initPlayerBillboard :: ECS ()
initPlayerBillboard = do
  let vertexShader   = shaderPath  </> "billboard.v.glsl"
      fragmentShader = shaderPath  </> "billboard.f.glsl"
      texture        = texturePath </> "player.tga"
      objFile        = objPath     </> "player.obj"
  -- load in shaders
  sp  <- liftIO $ U.simpleShaderProgram vertexShader fragmentShader
  -- load the obj file
  obj <- liftIO $ loadObjFile objFile
  -- load the image
  uv  <- liftIO $ getAndCreateTexture texture
  -- create the buffer related data
  vb  <- liftIO $ U.fromSource GL.ArrayBuffer $ _verts     obj
  uvb <- liftIO $ U.fromSource GL.ArrayBuffer $ _texCoords obj
  -- define the entity
  newEntity (
      Player
    , TexResource { _sProgram   = sp
                  , _texObj     = uv
                  , _vertBuffer = vb
                  , _uvBuffer   = uvb
                  , _objData    = obj }
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0)
    , Position3D $ L.V3 0 1 (-1) )
  return ()

drawPlayerBillboard :: (ProjectionMatrix, ViewMatrix) -> PlayerB -> IO ()
drawPlayerBillboard (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
                    (_, tr, Orientation o, Position3D cPos) = do
  let trans         = projMatrix  !*! viewMatrix
      shaderProgram = _sProgram   tr
      texture       = _texObj     tr
      vertices      = _vertBuffer tr
      uvCoords      = _uvBuffer   tr
      numVerts      = fromIntegral $ length $ _verts . _objData $ tr
      attribKeys    = keys $ U.attribs  shaderProgram
      uniformKeys   = keys $ U.uniforms shaderProgram
      program       = U.program shaderProgram
      posLoc        = U.getAttrib  shaderProgram "squareVertices"
      mtsLoc        = U.getUniform shaderProgram "myTextureSampler"
      vpLoc         = U.getUniform shaderProgram "VP"
      crwLoc        = U.getUniform shaderProgram "CameraRight_worldspace"
      cuwLoc        = U.getUniform shaderProgram "CameraUp_worldspace"
      bpLoc         = U.getUniform shaderProgram "BillboardPos"
      bsLoc         = U.getUniform shaderProgram "BillboardSize"
  -- set current program to shaderProgram
  GL.currentProgram              $= Just program
  -- enable all attributes
  GL.vertexAttribArray    posLoc $= GL.Enabled
  -- GL.vertexAttribArray    uvLoc  $= GL.Enabled
  -- handle uniforms
  -- bind texture to TextureUnit 0
  -- set "myTextureSampler" sampler to use Texture Unit 0
  GL.activeTexture               $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= texture
  GL.uniform mtsLoc              $= GL.Index1 (0 :: GL.GLint)
  -- set view-projection to camera vp
  trans      `U.asUniform` vpLoc
  -- set billboard pos to center of object position
  cPos       `U.asUniform` bpLoc
  -- set size of billboard (in world units)
  ((L.V2 1 2) :: L.V2 Float) `U.asUniform` bsLoc
  -- align billboard to camera right & up axes
  let L.V4
        (L.V4 vm00 vm01 _ _)
        (L.V4 vm10 vm11 _ _)
        (L.V4 vm20 vm21 _ _)
        _                    = viewMatrix
  (L.V3 vm00 vm10 vm20) `U.asUniform` crwLoc
  (L.V3 vm01 vm11 vm21) `U.asUniform` cuwLoc
  -- bind position VB
  GL.bindBuffer GL.ArrayBuffer   $= Just vertices
  GL.vertexAttribPointer  posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- bind uv coords VB
  GL.bindBuffer GL.ArrayBuffer   $= Just uvCoords
  -- GL.vertexAttribPointer  uvLoc  $=
  --   ( GL.ToFloat
  --   , GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0 )
  -- draw indexed triangles
  GL.drawArrays GL.TriangleStrip 0 numVerts
  -- disable all attributes
  GL.vertexAttribArray    posLoc $= GL.Disabled
  -- GL.vertexAttribArray    uvLoc  $= GL.Disabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer  $= Nothing
  -- unset current program
  GL.currentProgram             $= Nothing
