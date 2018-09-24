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
import           Game.Util.Constants     (objPath, texturePath, shaderPath, frameDeltaSeconds)
import           Game.Loaders.Obj.Loader (loadObjFile)
import           Game.Loaders.Program    (createProgram, getAttrib, getUniform)
import           Game.Loaders.Texture    (getAndCreateTexture)
import           Game.Util.Move          (Moveable)
import           Game.Util.Sprite        (loadSpriteSheet)
import           Game.Util.BufferObjects (fromSource, replaceBuffer)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , ShaderProgram(..)
  , Texture(..)
  , SpriteSheet(..)
  , AnimationClip
  , Animations
  , FrameInfo
  , AnimationKey(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , Player(..)
  , Seconds(..)
  , Step(..)
  , AnimAction(..)
  )

type PlayerB =
  ( Player
  , ShaderProgram
  , SpriteSheet
  , BufferResource
  , Moveable
  )

stepAction :: Step AnimAction -> Animations -> FrameInfo -> FrameInfo
stepAction (Step'Sustain _)   animations pos =
  A.stepPosition animations pos $ Seconds (realToFrac frameDeltaSeconds :: Float)
stepAction (Step'Change _ pa) _          pos = case pa of
  PlayerAction'Walk -> A.initPositionWithLoop PlayerKey'Walk A.Loop'Always
  PlayerAction'Idle -> A.initPosition         PlayerKey'Idle

verts :: [L.V3 Float]
verts = [ L.V3 (-0.5) (-0.5)  0.0 -- bottom left
        , L.V3 (-0.5)   0.5   0.0 -- top    left
        , L.V3   0.5    0.5   0.0 -- top    right
        , L.V3 (-0.5) (-0.5)  0.0 -- bottom left
        , L.V3   0.5    0.5   0.0 -- top    right
        , L.V3   0.5  (-0.5)  0.0 -- bottom right
        ]

uvVertices :: [L.V2 Float]
uvVertices = [ L.V2 0 1 -- bottom left
             , L.V2 0 0 -- top    left
             , L.V2 1 0 -- top    right
             , L.V2 0 1 -- bottom left
             , L.V2 1 0 -- top    right
             , L.V2 1 1 -- bottom right
             ]

texSizeToDims :: GL.TextureSize2D -> L.V2 Float
texSizeToDims (GL.TextureSize2D w h) = L.V2 (realToFrac w) (realToFrac h)

makeFrameUVs :: AnimationClip -> L.V2 Float -> [L.V2 Float]
makeFrameUVs sc (L.V2 w' h') =
    let scX :: Float = (realToFrac $ A.scX sc) / w'
        scY :: Float = (realToFrac $ A.scY sc) / h'
        scW :: Float = (realToFrac $ A.scW sc) / w'
        scH :: Float = (realToFrac $ A.scH sc) / h'
        topLeft      = L.V2 scX         (1 - scY)
        topRight     = L.V2 (scX + scW) (1 - scY)
        botLeft      = L.V2 scX         (1 - (scY + scH))
        botRight     = L.V2 (scX + scW) (1 - (scY + scH))
    in [ botLeft
       , topLeft
       , topRight
       , botLeft
       , topRight
       , botRight
       ]

initPlayerBillboard :: ECS ()
initPlayerBillboard = do
  let vertexShader   = shaderPath  </> "p_billboard.v.glsl"
      fragmentShader = shaderPath  </> "p_billboard.f.glsl"
      ssFilePath     = texturePath </> "doom_soldier.json"

  spriteSheet <- liftIO $ loadSpriteSheet ssFilePath
  let pSheet = SpriteSheet { _ssAction   = Step'Sustain PlayerAction'Walk
                           , _ssSheet    = spriteSheet
                           , _ssPosition = A.initPosition PlayerKey'Walk }

  -- load in shaders
  program <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]
  -- create the buffer related data
  vb  <- liftIO $ fromSource (GL.StaticDraw,  GL.ArrayBuffer) $ verts
  tx  <- liftIO $ fromSource (GL.DynamicDraw, GL.ArrayBuffer) $ uvVertices
  -- define the entity
  newEntity (
      Player
    , program
    , pSheet
    -- , Texture texObj
    , BufferResource { _vertexBuffer   = Just vb
                     , _texCoordBuffer = Just tx
                     , _normalBuffer   = Nothing
                     , _rgbCoordBuffer = Nothing
                     , _indexBuffer    = Nothing }
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0)
    , Position3D  $ L.V3 0 1 (-2) )
  return ()

stepPlayerBillboard :: (Player, SpriteSheet) -> SpriteSheet
stepPlayerBillboard (_, ss) =
  let animations = A.ssAnimations $ _ssSheet ss
      frameInfo  = _ssPosition ss
      action     = _ssAction   ss
  in ss { _ssPosition = stepAction action animations frameInfo }

drawPlayerBillboard :: (ProjectionMatrix, ViewMatrix) -> PlayerB -> IO ()
drawPlayerBillboard (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
                    (_, sProgram, pSheet, br, (Orientation o, Position3D mPos)) = do
  let modelMatrix = L.mkTransformationMat L.identity mPos
      trans       = projMatrix !*! viewMatrix !*! modelMatrix
      -- animate
      texObj      = A.ssImage      $ _ssSheet pSheet
      animations  = A.ssAnimations $ _ssSheet pSheet
      spriteClip  = A.currentLocation animations (_ssPosition pSheet)
      L.V2 tW tH  = texSizeToDims (_textureSize texObj)
      frameUVs    = makeFrameUVs spriteClip (L.V2 tW tH)
      bbSize      = L.V2 (2 * (tW / tH)) 2
      -- attribs & uniforms
      -- vertex shader
      posLoc      = getAttrib  sProgram "SquareVertices"
      texLoc      = getAttrib  sProgram "TextureCoords"
      viewMatLoc  = getUniform sProgram "ViewMatrix"
      projMatLoc  = getUniform sProgram "ProjMatrix"
      cameraRight = getUniform sProgram "CameraRight_worldspace"
      cameraUp    = getUniform sProgram "CameraUp_worldspace"
      bpLoc       = getUniform sProgram "BillboardPos"
      bsLoc       = getUniform sProgram "BillboardSize"
      -- fragment shader
      mtsLoc      = getUniform sProgram "TextureSampler"

  -- set current program to shaderProgram
  GL.currentProgram              $= Just (_glProgram sProgram)
  -- enable all attributes
  GL.vertexAttribArray    posLoc $= GL.Enabled
  GL.vertexAttribArray    texLoc $= GL.Enabled
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
  (L.V3 vm00 vm10 vm20) `U.asUniform` cameraRight
  (L.V3 vm01 vm11 vm21) `U.asUniform` cameraUp
  -- set billboard pos to center of object position
  mPos   `U.asUniform` bpLoc
  -- set size of billboard (in world units)
  bbSize `U.asUniform` bsLoc
  -- set view-projection to camera vp
  viewMatrix  `U.asUniform` viewMatLoc
  projMatrix  `U.asUniform` projMatLoc

  -- bind position VB ("squareVertices")
  GL.bindBuffer GL.ArrayBuffer   $= (_vertexBuffer br)
  GL.vertexAttribPointer  posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )

  -- bind texture coordinates ("TextureCoords")
  GL.bindBuffer GL.ArrayBuffer   $= (_texCoordBuffer br)
  replaceBuffer (GL.DynamicDraw, GL.ArrayBuffer) frameUVs
  GL.vertexAttribPointer  texLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0 )

  -- enable blending func (for transparency)
  GL.blend     $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- draw indexed triangles
  GL.drawArrays GL.TriangleStrip 0 6
  -- disable blending func
  GL.blend     $= GL.Disabled
  -- disable all attributes
  GL.vertexAttribArray posLoc  $= GL.Disabled
  GL.vertexAttribArray texLoc  $= GL.Disabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  -- unset current program
  GL.currentProgram            $= Nothing
