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
import           Foreign.Ptr             (nullPtr)

import           Game.World.TH           (ECS)
import           Game.Util.Constants     (shaderPath, texturePath)
import           Game.Loaders.Texture    (getAndCreateTexture)
import           Game.Util.GLError       (printGLErrors)
import           Game.Util.BufferObjects (fromSource)
import           Game.Loaders.Program    (createProgram, getAttrib, getUniform)
import           Game.Util.Move          (Moveable)
import           Game.Util.Terrain       (generateTerrain)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ShaderProgram(..)
  , Texture(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , Terrain(..)
  , TerrainInfo(..)
  , TerrainConfig(..)
  , Player(..) )

type TerrainE = (Terrain, Texture, ShaderProgram, BufferResource, Moveable)

initTerrain :: ECS ()
initTerrain = do
  let vertexShader   = shaderPath  </> "terrain.v.glsl"
      fragmentShader = shaderPath  </> "terrain.f.glsl"
      texFilePath    = texturePath </> "terrain.tga"
      terrainConfig  = TerrainConfig { _trSize      = 20
                                     , _trVertCount = 32 }
      tr             = generateTerrain terrainConfig
      halfSize       = (_trSize terrainConfig) / 2

  terrainTexture <- liftIO $ getAndCreateTexture texFilePath
  -- load in shaders
  program <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]
  -- create the buffer related data
  vertices  <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) $ _trVertices tr
  texCoords <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) $ _trTexCoords tr
  normals   <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) $ _trNormals tr
  indices   <- liftIO $ fromSource (GL.StaticDraw, GL.ElementArrayBuffer) $ _trIndices tr

  -- bind element buffer
  liftIO $ GL.bindBuffer GL.ElementArrayBuffer $= Just indices
  liftIO $ printGLErrors "renderTerrain bind element buffer"

  -- define the entity
  newEntity (
      Terrain terrainConfig
    , terrainTexture
    , program
    , BufferResource { _vertexBuffer   = Just vertices
                     , _texCoordBuffer = Just texCoords
                     , _normalBuffer   = Just normals
                     , _rgbCoordBuffer = Nothing
                     , _indexBuffer    = Just indices }
    , ( Orientation $ L.Quaternion 1 (L.V3 0 0 0)
      , Position3D  $ L.V3 (-halfSize) 0 (-halfSize) ) )
  return ()

sun :: L.V3 Float
sun = L.V3 20000 20000 2000

sunColor :: L.V3 Float
sunColor = L.V3 1 1 1

shineDamper :: Float
shineDamper = 1

reflectivity :: Float
reflectivity = 0

drawTerrain :: (ProjectionMatrix, ViewMatrix) -> TerrainE -> IO ()
drawTerrain (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
            (tr, texture, sProgram, br, (Orientation o, Position3D mPos)) = do
  let transMatrix      = L.mkTransformationMat L.identity mPos
      -- vertex shader attrib locations
      positionLocation = getAttrib sProgram "position"
      texCoordLocation = getAttrib sProgram "texCoords"
      normalLocation   = getAttrib sProgram "normal"
      -- vertex shader uniform locations
      transMatLocation = getUniform sProgram "transformationMatrix"
      projMatLocation  = getUniform sProgram "projectionMatrix"
      viewMatLocation  = getUniform sProgram "viewMatrix"
      lightPosLocation = getUniform sProgram "lightPosition"
      -- fragment shader uniform locations
      texSamplerLocation   = getUniform sProgram "terrainTexture"
      lightColorLocation   = getUniform sProgram "lightColor"
      shineDamperLocation  = getUniform sProgram "shineDamper"
      reflectivityLocation = getUniform sProgram "reflectivity"
      -- terrain config
      Terrain tConfig = tr
      tSize = _trSize tConfig

  -- set current program to shaderProgram
  GL.currentProgram $= Just (_glProgram sProgram)
  printGLErrors "renderTerrain set program"

  -- enable all attributes
  GL.vertexAttribArray positionLocation $= GL.Enabled
  GL.vertexAttribArray texCoordLocation $= GL.Enabled
  GL.vertexAttribArray normalLocation   $= GL.Enabled
  printGLErrors "renderTerrain enable attribs"

  -- handle uniforms
  transMatrix  `U.asUniform` transMatLocation
  projMatrix   `U.asUniform` projMatLocation
  viewMatrix   `U.asUniform` viewMatLocation
  sun          `U.asUniform` lightPosLocation
  sunColor     `U.asUniform` lightColorLocation
  shineDamper  `U.asUniform` shineDamperLocation
  reflectivity `U.asUniform` reflectivityLocation

  -- sampler2D uniform
  GL.activeTexture               $= GL.TextureUnit 4
  GL.textureBinding GL.Texture2D $= (_textureId texture)
  GL.uniform texSamplerLocation  $= GL.Index1 (4 :: GL.GLint)
  printGLErrors "renderTerrain handle uniforms"

  -- send attribs to shaders
  -- send position vertices to shader
  GL.bindBuffer GL.ArrayBuffer  $= (_vertexBuffer br)
  GL.vertexAttribPointer positionLocation $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- send texture coordinate vertices to shader
  GL.bindBuffer GL.ArrayBuffer  $= (_texCoordBuffer br)
  GL.vertexAttribPointer texCoordLocation $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0 )
  -- send normal vertices to shader
  GL.bindBuffer GL.ArrayBuffer  $= (_normalBuffer br)
  GL.vertexAttribPointer normalLocation $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  printGLErrors "renderTerrain set attrib pointers"

  -- bind element buffer
  GL.bindBuffer GL.ElementArrayBuffer $= (_indexBuffer br)
  printGLErrors "renderTerrain bind element buffer"

  -- draw triangles
  GL.drawElements GL.Triangles (floor $ 3 * 6 * (tSize * tSize)) GL.UnsignedInt nullPtr
  printGLErrors "renderTerrain draw triangles"

  -- disable all attributes
  GL.vertexAttribArray positionLocation $= GL.Disabled
  GL.vertexAttribArray texCoordLocation $= GL.Disabled
  GL.vertexAttribArray normalLocation   $= GL.Disabled
  printGLErrors "renderTerrain disable attribs"

  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  printGLErrors "renderTerrain unbind array buffer"

  -- unbind element buffer
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  printGLErrors "renderTerrain unbind element buffer"

  -- unset current program
  GL.currentProgram $= Nothing
  printGLErrors "renderTerrain unset program"
