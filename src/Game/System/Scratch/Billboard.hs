module Game.System.Scratch.Billboard where

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

import           Game.Util.Constants     (texturePath, shaderPath)
import           Game.Loaders.Program       (createProgram, getAttrib, getUniform)
import           Game.Loaders.Texture       (getAndCreateTexture)
import           Game.Util.BufferObjects (fromSource)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ShaderProgram(..)
  , Texture(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , Billboard(..)
  , Player(..) )

type RenderBillboard =
  ( Billboard
  , ShaderProgram
  , BufferResource
  , Position3D )

verts :: [L.V3 Float]
verts = [
    L.V3 (-0.5) (-0.5)  0.0
  , L.V3   0.5  (-0.5)  0.0
  , L.V3 (-0.5)   0.5   0.0
  , L.V3   0.5    0.5   0.0
  ]

initBillboards :: IO [RenderBillboard]
initBillboards = do
  let vertexShader   = shaderPath  </> "billboard.v.glsl"
      fragmentShader = shaderPath  </> "billboard.f.glsl"

  -- load in shaders
  program <- createProgram [ ShaderInfo GL.VertexShader   vertexShader
                           , ShaderInfo GL.FragmentShader fragmentShader ]
  -- create the buffer related data
  vb  <- fromSource (GL.StaticDraw, GL.ArrayBuffer) $ verts
  -- define the entities
  return
    [ ( Billboard
      , program
      , BufferResource { _vertexBuffer   = Just vb
                       , _texCoordBuffer = Nothing
                       , _normalBuffer   = Nothing
                       , _rgbCoordBuffer = Nothing
                       , _indexBuffer    = Nothing }
      , Position3D  $ L.V3 1 0.5 (-6) )

    , ( Billboard
      , program
      , BufferResource { _vertexBuffer   = Just vb
                       , _texCoordBuffer = Nothing
                       , _normalBuffer   = Nothing
                       , _rgbCoordBuffer = Nothing
                       , _indexBuffer    = Nothing }
      , Position3D  $ L.V3 2 0.5 (-8) )
    ]

drawBillboard :: (ProjectionMatrix, ViewMatrix) -> RenderBillboard -> IO ()
drawBillboard (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
              (_, sProgram, br, Position3D mPos) = do
  let modelMatrix   = L.mkTransformationMat L.identity mPos
      trans         = projMatrix !*! viewMatrix !*! modelMatrix
      posLoc        = getAttrib  sProgram "squareVertices"
      vpLoc         = getUniform sProgram "VP"
      crwLoc        = getUniform sProgram "CameraRight_worldspace"
      cuwLoc        = getUniform sProgram "CameraUp_worldspace"
      bpLoc         = getUniform sProgram "BillboardPos"
      bsLoc         = getUniform sProgram "BillboardSize"
      ecLoc         = getUniform sProgram "entityColor"
  -- set current program to shaderProgram
  GL.currentProgram              $= Just (_glProgram sProgram)
  -- enable all attributes
  GL.vertexAttribArray    posLoc $= GL.Enabled
  -- handle uniforms
  -- align billboard to camera right & up axes
  let L.V4
        (L.V4 vm00 vm01 _ _)
        (L.V4 vm10 vm11 _ _)
        (L.V4 vm20 vm21 _ _)
        _                    = viewMatrix
  (L.V3 vm00 vm10 vm20) `U.asUniform` crwLoc
  (L.V3 vm01 vm11 vm21) `U.asUniform` cuwLoc
  -- set billboard pos to center of object position
  mPos  `U.asUniform` bpLoc
  -- set size of billboard (in world units)
  (L.V2 1 2 :: L.V2 Float) `U.asUniform` bsLoc
  -- set view-projection to camera vp
  trans `U.asUniform` vpLoc
  -- set color of billboard
  (L.V3 1 1 0 :: L.V3 Float) `U.asUniform` ecLoc
  -- bind position VB
  GL.bindBuffer GL.ArrayBuffer   $= (_vertexBuffer br)
  GL.vertexAttribPointer  posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- -- enable blending func (for transparency)
  -- GL.blend     $= GL.Enabled
  -- GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- draw indexed triangles
  GL.drawArrays GL.TriangleStrip 0 4
  -- disable blending func
  -- GL.blend     $= GL.Disabled
  -- disable all attributes
  GL.vertexAttribArray posLoc $= GL.Disabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer  $= Nothing
  -- unset current program
  GL.currentProgram             $= Nothing
