module Game.System.Scratch.DebugHUD where

import qualified Graphics.GLUtil           as U (asUniform)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           SDL                     (($=))
import           System.FilePath         ((</>))
import           Control.Monad           (mapM, foldM_)
import           Data.Bits               (shift)
import           Data.Map                (fromList, update, (!))
import           Data.List               (intercalate)
import           Data.Char               (ord)
import           Data.Int                (Int32)
import           Apecs                   (newEntity, cmap, cmapM_, set)

import           Game.World.TH           (ECS)
import           Game.Effect.Clock       (getGlobalTime)
import           Game.Util.Constants     (fontPath, shaderPath)
import           Game.Loaders.Program    (createProgram, getAttrib, getUniform)
import           Game.Loaders.Texture    (getAndCreateTexture)
import           Game.Util.GLError       (printGLErrors)
import           Game.Loaders.Font       (loadCharacters)
import           Game.Util.BufferObjects (fromSource, replaceBuffer, offset0)
import           Game.Util.Camera        (CameraEntity)
import           Game.Types
  ( GlobalTime(..)
  , ShaderProgram(..)
  , DebugHUD(..)
  , HUDType(..)
  , HUDInfo(..)
  , FontMap(..)
  , Character(..)
  , FontInfo(..)
  , BufferResource(..)
  , Texture(..)
  , ShaderInfo(..)
  , Position3D(..) )

type DebugHUDEntity = (DebugHUD, ShaderProgram, BufferResource)

characters :: String
characters =
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ [' ', ':', ',', '-', '.', '>', '<', '(', ')']

uvVertices :: [L.V2 Float]
uvVertices =
  [ L.V2 0 0
  , L.V2 0 1
  , L.V2 1 1
  , L.V2 0 0
  , L.V2 1 1
  , L.V2 1 0 ]

textureUnit :: GL.TextureUnit
textureUnit = GL.TextureUnit 2

initDebugHUD :: IO [DebugHUDEntity]
initDebugHUD = do
  let vertexShader   = shaderPath </> "debug_hud.v.glsl"
      fragmentShader = shaderPath </> "debug_hud.f.glsl"
      fontFilePath   = fontPath   </> "Roboto-Regular.ttf"
  -- load in shaders
  program <- createProgram [ ShaderInfo GL.VertexShader   vertexShader
                           , ShaderInfo GL.FragmentShader fragmentShader ]
  -- load the font
  fontMap <- loadCharacters fontFilePath 48 textureUnit characters
  -- create the buffer related data
  -- first we want to allocate an array of vertices with the proper size
  -- in terms of memory, but we don't care about the actual values of the
  -- vertices yet, so we'll just create a junk array via list comprehension
  vertsBuf <- fromSource
    (GL.DynamicDraw, GL.ArrayBuffer)
    [ (L.V2 0 0) :: L.V2 Float | _ <- [0..5] ]
  uvBuf    <- fromSource
    (GL.StaticDraw, GL.ArrayBuffer)
    uvVertices
  -- note, x & y positions are [-1/2 screen, 1/2 screen] both axes,
  -- 0,0 is the center
  let fpsInfo = FontInfo { _fText = "Fps: 0"
                         , _fxPos = 10
                         , _fyPos = 550
                         , _fSize = 0.5 }
      posInfo = FontInfo { _fText = "Pos: V3 0 0 0"
                         , _fxPos = 10
                         , _fyPos = 500
                         , _fSize = 0.5 }
  -- define the entity
  return [
      ( DebugHUD { _hudInfo = HUDInfo $ fromList [ (FPSCounter,      fpsInfo)
                                                 , (PositionTracker, posInfo) ]
                 , _fontMap = fontMap }
      , program
      , BufferResource { _vertexBuffer   = Just vertsBuf
                       , _texCoordBuffer = Just uvBuf
                       , _normalBuffer   = Nothing
                       , _rgbCoordBuffer = Nothing
                       , _indexBuffer    = Nothing  }
      )
    ]

updatePosText :: Position3D -> FontInfo -> Maybe FontInfo
updatePosText (Position3D (L.V3 x y z)) fi =
  let posText = "(" ++ intercalate ", " (map (take 3 . show) [x, y, z]) ++ ")"
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
    cmap $ \((dHud, _, _) :: DebugHUDEntity) ->
      let (HUDInfo dMap) = _hudInfo dHud
      in dHud { _hudInfo = HUDInfo $ (update (updatePosText cPos)  PositionTracker)
                                   . (update (updateFpsText fpsDt) FPSCounter)
                                   $ dMap }

-- makeFontVertices :: FontInfo -> ([L.V2 Float], [L.V2 Float])
-- makeFontVertices fontInfo =
--   let text          = _fText fontInfo
--       x             = _fxPos fontInfo
--       y             = _fyPos fontInfo
--       size          = _fSize fontInfo
--       length'       = length $ _fText fontInfo
--       (verts, uvxs) = unzip
--         [ (map (fmap realToFrac) vertices, uvs)
--         | i <- [0..(length text - 1)]
--         -- for vertex coords
--         , let iSize      = i * size
--               vtxUpLeft  = L.V2 (x + iSize)        (y + size)
--               vtxUpRight = L.V2 (x + iSize + size) (y + size)
--               vtxDnRight = L.V2 (x + iSize + size) (y)
--               vtxDnLeft  = L.V2 (x + iSize)        (y)
--               vertices   = [ vtxUpLeft,  vtxDnLeft,  vtxUpRight
--                            , vtxDnRight, vtxUpRight, vtxDnLeft ]
--         -- for uv coords
--               chr       = ord $ text !! i
--               i16       = 1 / 16 :: Float
--               uxX       = (realToFrac $ chr `mod` 16 :: Float) / 16
--               uvY       = (realToFrac $ chr `div` 16 :: Float) / 16
--               uvUpLeft  = L.V2 (uxX)       (uvY)
--               uvUpRight = L.V2 (uxX + i16) (uvY)
--               uvDnRight = L.V2 (uxX + i16) (uvY + i16)
--               uvDnLeft  = L.V2 (uxX)       (uvY + i16)
--               uvs       = [ uvUpLeft,  uvDnLeft,  uvUpRight
--                           , uvDnRight, uvUpRight, uvDnLeft ] ]
--   in (concat verts, concat uvxs)

drawDebugHUD :: DebugHUDEntity -> (L.V2 Int32) -> HUDType -> IO ()
drawDebugHUD (hud, sProgram, br) dims hudType = do
  let (HUDInfo hudInfo) = _hudInfo hud
      (FontMap fontMap) = _fontMap hud
      fontInfo          = hudInfo ! hudType
      program           = _glProgram sProgram
      vpLocation        = getAttrib  sProgram "vertexPosition_screenSpace"
      uvLocation        = getAttrib  sProgram "vertexUV"
      sampLocation      = getUniform sProgram "fontTextureSampler"
      dimsLocation      = getUniform sProgram "dims"
      posBuffer         = _vertexBuffer   br
      uvBuffer          = _texCoordBuffer br
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

  -- enable blending func
  GL.blend     $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  printGLErrors "drawDebugHUD set blend"

  -- set "fontTextureSampler" uniform
  GL.activeTexture               $= textureUnit
  GL.uniform sampLocation        $= GL.Index1 (2 :: GL.GLint)
  printGLErrors "drawDebugHUD texture uniform"

  foldM_ (\advAcc chr -> do
    let character              = fontMap ! chr
        scale                  = _fSize fontInfo
        texture                = _charTexture character
        GL.TextureSize2D sW sH = _textureSize texture
        L.V2 bX bY             = _charBearing character
    -- generate new buffer coordinates here
        sX                     = realToFrac sW
        sY                     = realToFrac sH
        xPos                   = (_fxPos fontInfo + advAcc) + (bX * scale)
        yPos                   = (_fyPos fontInfo) - ((sY - bY) * scale)
        w                      = sX * scale
        h                      = sY * scale
    -- new VBO vertices
        verts                  = [ L.V2 xPos       (yPos + h)   -- bottom left
                                 , L.V2 xPos       yPos         -- top left
                                 , L.V2 (xPos + w) yPos         -- top right
                                 , L.V2 xPos       (yPos + h)   -- bottom left
                                 , L.V2 (xPos + w) yPos         -- top right
                                 , L.V2 (xPos + w) (yPos + h) ] -- bottom right

    -- bind to current letter texture
    GL.textureBinding GL.Texture2D $= (_textureId texture)

    -- replace vertex buffer with new coords
    GL.bindBuffer GL.ArrayBuffer      $= posBuffer
    replaceBuffer (GL.DynamicDraw, GL.ArrayBuffer) verts
    GL.vertexAttribPointer vpLocation $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 2 GL.Float 0 offset0 )
    printGLErrors "drawDebugHUD position buffer"

    -- replace UV buffer with new coords
    GL.bindBuffer GL.ArrayBuffer      $= uvBuffer
    GL.vertexAttribPointer uvLocation $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 2 GL.Float 0 offset0 )
    printGLErrors "drawDebugHUD uv buffer"
    -- -- draw indexed triangles
    GL.drawArrays GL.Triangles 0 6
    printGLErrors "drawDebugHUD draw"
    -- accumulate advance offset (remember it's in 1/64 pixels, so divide it)
    return $ advAcc + (((realToFrac $ _charAdvance character) / 64) * scale)
    ) 0 $ _fText fontInfo

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
  return ()
