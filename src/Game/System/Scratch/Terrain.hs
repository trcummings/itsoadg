module Game.System.Scratch.Terrain where

import qualified Graphics.GLUtil           as U
import qualified Graphics.GLUtil.Camera3D  as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           Linear                  ((!*!))
import           SDL                     (($=))
import           Control.Monad           (mapM_)
import           System.FilePath         ((</>))
import           Control.Monad.IO.Class  (liftIO)
import           Apecs                   (newEntity)

import           Game.World.TH           (ECS)
import           Game.Util.Constants     (shaderPath)
import           Game.Util.Program       (createProgram, getAttrib, getUniform)
import           Game.Util.Terrain       (generateTerrain, TerrainInfo(..), vertexCount)
import           Game.Util.Move          (Moveable)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ShaderProgram(..)
  -- , Texture(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , Terrain(..)
  , Player(..) )

type TerrainE = (Terrain, ShaderProgram, BufferResource, Moveable)

initTerrain :: ECS ()
initTerrain = do
  let vertexShader   = shaderPath  </> "terrain.v.glsl"
      fragmentShader = shaderPath  </> "terrain.f.glsl"
      tr             = generateTerrain

  liftIO $ putStrLn $ show $ length $ _trVertices tr
  -- load in shaders
  program <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]
  -- create the buffer related data
  vertices  <- liftIO $ U.fromSource GL.ArrayBuffer $ _trVertices tr
  texCoords <- liftIO $ U.fromSource GL.ArrayBuffer $ _trTexCoords tr
  normals   <- liftIO $ U.fromSource GL.ArrayBuffer $ _trNormals tr
  indices   <- liftIO $ U.fromSource GL.ElementArrayBuffer $ _trIndices tr
  -- define the entity
  newEntity (
      Terrain
    , program
    , BufferResource { _vertexBuffer   = Just vertices
                     , _texCoordBuffer = Just texCoords
                     , _normalBuffer   = Just normals
                     , _rgbCoordBuffer = Nothing
                     , _indexBuffer    = Just indices }
    , ( Orientation $ L.Quaternion 1 (L.V3 0 0 0)
      , Position3D  $ L.V3 0 0 (-10) ) )
  return ()

drawTerrain :: (ProjectionMatrix, ViewMatrix) -> TerrainE -> IO ()
drawTerrain (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
            (_, sProgram, br, (Orientation o, Position3D mPos)) = do
  let transMatrix = L.mkTransformation o mPos
      posLoc      = getAttrib sProgram "position"
  -- set current program to shaderProgram
  GL.currentProgram $= Just (_glProgram sProgram)
  -- enable all attributes
  GL.vertexAttribArray posLoc $= GL.Enabled
  -- handle uniforms
  transMatrix `U.asUniform` (getUniform sProgram "transformationMatrix")
  projMatrix  `U.asUniform` (getUniform sProgram "projectionMatrix")
  viewMatrix  `U.asUniform` (getUniform sProgram "viewMatrix")
  -- send attribs to shaders
  GL.bindBuffer GL.ArrayBuffer  $= (_vertexBuffer br)
  GL.vertexAttribPointer posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- bind element buffer
  -- GL.bindBuffer GL.ElementArrayBuffer $= (_indexBuffer br)
  -- draw indexed triangles
  GL.drawArrays GL.Triangles 0 16384
  -- disable all attributes
  GL.vertexAttribArray posLoc $= GL.Enabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  -- unbind element buffer
  -- GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  -- unset current program
  GL.currentProgram $= Nothing
