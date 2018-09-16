module Game.System.Scratch.DebugHUD where

import qualified Graphics.GLUtil           as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           SDL                     (($=))
import           System.FilePath         ((</>))
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map                (fromList, update)
import           Data.Char               (ord)
import           Data.Int                (Int32)
import           Apecs                   (newEntity, cmap, cmapM_, set)

import           Game.World.TH       (ECS)
import           Game.Effect.Clock   (getGlobalTime)
import           Game.Util.Constants (texturePath, shaderPath)
import           Game.Util.Program   (createProgram, getAttrib, getUniform)
import           Game.Util.Texture   (getAndCreateTexture)
import           Game.Util.GLError   (printGLErrors)
import           Game.Util.Camera    (CameraEntity)
import           Game.Types
  ( GlobalTime(..)
  , ShaderProgram(..)
  , DebugHUD(..)
  , HUDType(..)
  , FontInfo(..)
  , BufferResource(..)
  , Texture(..)
  , ShaderInfo(..)
  , Position3D(..) )

type DebugHUDEntity = (DebugHUD, ShaderProgram, Texture, BufferResource)

initDebugHUD :: ECS ()
initDebugHUD = do
  let vertexShader   = shaderPath  </> "debug_hud.v.glsl"
      fragmentShader = shaderPath  </> "debug_hud.f.glsl"
      fontTexture    = texturePath </> "font.tga"
  -- load in shaders
  program <- liftIO $
    createProgram [ ShaderInfo GL.VertexShader   vertexShader
                  , ShaderInfo GL.FragmentShader fragmentShader ]
  -- load the image
  texObj   <- liftIO $ getAndCreateTexture fontTexture
  -- create the buffer related data
  vertsBuf <- liftIO $ U.fromSource GL.ArrayBuffer ([] :: [L.V2 Float])
  uvBuf    <- liftIO $ U.fromSource GL.ArrayBuffer ([] :: [L.V2 Float])
  -- note, x & y positions are [-1/2 screen, 1/2 screen] both axes,
  -- 0,0 is the center
  let fpsInfo = FontInfo { _fText = "Fps: 0"
                         , _fxPos = 10
                         , _fyPos = 550
                         , _fSize = 20 }
      posInfo = FontInfo { _fText = "Pos: V3 0 0 0"
                         , _fxPos = 10
                         , _fyPos = 500
                         , _fSize = 20 }
  -- define the entity
  newEntity
    ( DebugHUD $ fromList [ (FPSCounter,      fpsInfo)
                          , (PositionTracker, posInfo) ]
    , Texture texObj
    , BufferResource { _vertexBuffer   = Just vertsBuf
                     , _texCoordBuffer = Just uvBuf
                     , _normalBuffer   = Nothing
                     , _rgbCoordBuffer = Nothing
                     , _indexBuffer    = Nothing  }
    , program )
  return ()

updatePosText :: Position3D -> FontInfo -> Maybe FontInfo
updatePosText (Position3D cPos) fi =
  let posText = show $ floor <$> cPos
  in Just $ fi { _fText = "Pos: " ++ posText }

updateFpsText :: String -> FontInfo -> Maybe FontInfo
updateFpsText str fi = Just $ fi { _fText = "Fps: " ++ str }


stepDebugHUD :: ECS ()
stepDebugHUD = do
  -- get past time & current time
  (GlobalTime { _pastTime    = pTime
              , _currentTime = cTime }) <- getGlobalTime
  let fpsDt = take 4 $ show (1 / ((cTime - pTime) / 1000 ))
  -- get camera position
  cmapM_ $ \((_, (_, cPos)) :: CameraEntity) -> do
    cmap $ \((DebugHUD dMap, _, _, _) :: DebugHUDEntity) ->
      DebugHUD $ (update (updatePosText cPos) PositionTracker)
               . (update (updateFpsText fpsDt) FPSCounter)
               $ dMap

makeFontVertices :: FontInfo -> ([L.V2 Float], [L.V2 Float])
makeFontVertices fontInfo =
  let text          = _fText fontInfo
      x             = _fxPos fontInfo
      y             = _fyPos fontInfo
      size          = _fSize fontInfo
      length'       = length $ _fText fontInfo
      (verts, uvxs) = unzip
        [ (map (fmap realToFrac) vertices, uvs)
        | i <- [0..(length text - 1)]
        -- for vertex coords
        , let iSize      = i * size
              vtxUpLeft  = L.V2 (x + iSize)        (y + size)
              vtxUpRight = L.V2 (x + iSize + size) (y + size)
              vtxDnRight = L.V2 (x + iSize + size) (y)
              vtxDnLeft  = L.V2 (x + iSize)        (y)
              vertices   = [ vtxUpLeft,  vtxDnLeft,  vtxUpRight
                           , vtxDnRight, vtxUpRight, vtxDnLeft ]
        -- for uv coords
              chr       = ord $ text !! i
              i16       = 1 / 16 :: Float
              uxX       = (realToFrac $ chr `mod` 16 :: Float) / 16
              uvY       = (realToFrac $ chr `div` 16 :: Float) / 16
              uvUpLeft  = L.V2 (uxX)       (uvY)
              uvUpRight = L.V2 (uxX + i16) (uvY)
              uvDnRight = L.V2 (uxX + i16) (uvY + i16)
              uvDnLeft  = L.V2 (uxX)       (uvY + i16)
              uvs       = [ uvUpLeft,  uvDnLeft,  uvUpRight
                          , uvDnRight, uvUpRight, uvDnLeft ] ]
  in (concat verts, concat uvxs)

drawDebugHUD :: (ShaderProgram, Texture, BufferResource)
             -> (L.V2 Int32) -> FontInfo -> IO ()
drawDebugHUD (sProgram, Texture texObj, br) dims fontInfo = do
  let program      = _glProgram sProgram
      vpLocation   = getAttrib  sProgram "vertexPosition_screenSpace"
      uvLocation   = getAttrib  sProgram "vertexUV"
      sampLocation = getUniform sProgram "fontTextureSampler"
      dimsLocation = getUniform sProgram "dims"
      posBuffer    = _vertexBuffer br
      uvBuffer     = _texCoordBuffer br
      (verts, uvs) = makeFontVertices fontInfo
  -- clear the depth buffer
  GL.clear [GL.DepthBuffer]
  printGLErrors "drawDebugHUD clear depth buffer"
  -- set current program to shaderProgram
  GL.currentProgram               $= Just program
  printGLErrors "drawDebugHUD set program"
  -- enable all attributes
  GL.vertexAttribArray vpLocation $= GL.Enabled
  GL.vertexAttribArray uvLocation $= GL.Enabled
  printGLErrors "drawDebugHUD set attribs"
  -- handle uniforms
  -- set "dims" to viewport dims
  (realToFrac <$> dims :: L.V2 Float) `U.asUniform` dimsLocation
  -- set "fontTextureSampler" uniform
  GL.activeTexture               $= GL.TextureUnit 2
  GL.textureBinding GL.Texture2D $= texObj
  GL.uniform sampLocation        $= GL.Index1 (2 :: GL.GLint)
  printGLErrors "drawDebugHUD texture uniform"

  -- replace vertex buffer with new coords
  GL.bindBuffer   GL.ArrayBuffer    $= posBuffer
  U.replaceBuffer GL.ArrayBuffer verts
  GL.vertexAttribPointer vpLocation $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0 )
  printGLErrors "drawDebugHUD position buffer"

  -- replace UV buffer with new coords
  GL.bindBuffer   GL.ArrayBuffer    $= uvBuffer
  U.replaceBuffer GL.ArrayBuffer uvs
  GL.vertexAttribPointer uvLocation $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0 )
  printGLErrors "drawDebugHUD uv buffer"

  -- enable blending func
  GL.blend     $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  printGLErrors "drawDebugHUD set blend"
  -- -- draw indexed triangles
  GL.drawArrays GL.Triangles 0 (fromIntegral $ length verts)
  printGLErrors "drawDebugHUD draw"
  -- disable blending func
  GL.blend     $= GL.Disabled
  -- disable all attributes
  GL.vertexAttribArray vpLocation $= GL.Disabled
  GL.vertexAttribArray uvLocation $= GL.Disabled
  -- unbind array buffer
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  -- unset current program
  GL.currentProgram            $= Nothing
  printGLErrors "drawDebugHUD clean up"
