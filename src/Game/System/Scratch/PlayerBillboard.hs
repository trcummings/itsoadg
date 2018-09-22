{-# LANGUAGE NamedFieldPuns #-}

module Game.System.Scratch.PlayerBillboard where

import qualified Graphics.GLUtil           as U
import qualified Graphics.GLUtil.Camera3D  as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import qualified Animate                   as A
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
import           Game.Loaders.Program    (createProgram, getAttrib, getUniform)
import           Game.Loaders.Texture    (getAndCreateTexture)
import           Game.Util.Move          (Moveable)
import           Game.Util.Sprite        (loadSpriteSheet)
import           Game.Util.BufferObjects (fromSource)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ShaderProgram(..)
  , Texture(..)
  , SpriteSheet(..)
  , AnimationKey(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , Player(..) )

type PlayerB =
  ( Player
  , ShaderProgram
  , SpriteSheet
  , BufferResource
  , Moveable )

verts :: [L.V3 Float]
verts = [
    L.V3 (-0.5) (-0.5)  0.0
  , L.V3   0.5  (-0.5)  0.0
  , L.V3 (-0.5)   0.5   0.0
  , L.V3   0.5    0.5   0.0
  ]

initPlayerBillboard :: ECS ()
initPlayerBillboard = do
  let vertexShader   = shaderPath  </> "p_billboard.v.glsl"
      fragmentShader = shaderPath  </> "p_billboard.f.glsl"
      ssFilePath     = texturePath </> "doom_soldier.json"

  spriteSheet <- liftIO $ loadSpriteSheet ssFilePath
  let pSheet = SpriteSheet { _ssSheet    = spriteSheet
                           , _ssPosition = A.initPosition PlayerKey'Idle }


  -- load in shaders
  program <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]
  -- create the buffer related data
  vb  <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) $ verts
  -- define the entity
  newEntity (
      Player
    , program
    , pSheet
    -- , Texture texObj
    , BufferResource { _vertexBuffer   = Just vb
                     , _texCoordBuffer = Nothing
                     , _normalBuffer   = Nothing
                     , _rgbCoordBuffer = Nothing
                     , _indexBuffer    = Nothing }
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0)
    , Position3D  $ L.V3 0 0 (-1) )
  return ()

drawPlayerBillboard :: (ProjectionMatrix, ViewMatrix) -> PlayerB -> IO ()
drawPlayerBillboard (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
                    (_, sProgram, pSheet, br, (Orientation o, Position3D mPos)) = do
  let modelMatrix    = L.mkTransformationMat L.identity mPos
      trans          = projMatrix !*! viewMatrix !*! modelMatrix
      -- animate
      texObj                       = A.ssImage $ _ssSheet pSheet
      animations                   = A.ssAnimations (_ssSheet pSheet)
      A.SpriteClip { A.scX = scX
                   , A.scY = scY } = A.currentLocation animations (_ssPosition pSheet)
      GL.TextureSize2D w h         = _textureSize texObj
      frameX                       = ((realToFrac scX) / (realToFrac w)) :: Float
      frameY                       = ((realToFrac scY) / (realToFrac h)) :: Float
      -- attribs & uniforms
      -- framePos       = getAttrib  sProgram "framePos"
      posLoc         = getAttrib  sProgram "squareVertices"
      mtsLoc         = getUniform sProgram "myTextureSampler"
      vpLoc          = getUniform sProgram "VP"
      crwLoc         = getUniform sProgram "CameraRight_worldspace"
      cuwLoc         = getUniform sProgram "CameraUp_worldspace"
      bpLoc          = getUniform sProgram "BillboardPos"
      bsLoc          = getUniform sProgram "BillboardSize"
      txLoc          = getUniform sProgram "TextureCoords"
  -- set current program to shaderProgram
  GL.currentProgram              $= Just (_glProgram sProgram)
  -- enable all attributes
  GL.vertexAttribArray    posLoc $= GL.Enabled
  -- handle uniforms
  -- bind texture to TextureUnit 0
  -- set "myTextureSampler" sampler to use Texture Unit 1
  GL.activeTexture               $= GL.TextureUnit 1
  GL.textureBinding GL.Texture2D $= (_textureId texObj)
  GL.uniform mtsLoc              $= GL.Index1 (1 :: GL.GLint)
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
  ((L.V2 1 2) :: L.V2 Float) `U.asUniform` bsLoc
  -- set view-projection to camera vp
  trans `U.asUniform` vpLoc
  -- set coordinates of the frame's slice of the sprite sheet
  -- (L.V2 frameX frameY) `U.asUniform` txLoc

  -- bind position VB
  GL.bindBuffer GL.ArrayBuffer   $= (_vertexBuffer br)
  GL.vertexAttribPointer  posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- enable blending func (for transparency)
  GL.blend     $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- draw indexed triangles
  GL.drawArrays GL.TriangleStrip 0 4
  -- disable blending func
  GL.blend     $= GL.Disabled
  -- disable all attributes
  GL.vertexAttribArray posLoc $= GL.Disabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer  $= Nothing
  -- unset current program
  GL.currentProgram             $= Nothing
