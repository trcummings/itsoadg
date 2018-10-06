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
import           Game.Loaders.Font       (loadCharacters)
import           Game.Util.BufferObjects (fromSource, replaceBuffer, offset0)
import           Game.Util.Camera        (CameraEntity)
import           Game.Types
  ( GlobalTime(..)
  , ShaderProgram(..)
  , ProgramName(..)
  , ProgramMap(..)
  , DebugHUD(..)
  , HUDType(..)
  , HUDInfo(..)
  , FontMap(..)
  , Character(..)
  , FontInfo(..)
  , Player(..)
  , Facing(..)
  , BufferResource(..)
  , Texture(..)
  , ShaderInfo(..)
  , Position3D(..)
  , ProgramName(..)
  , RenderGlobals(..)
  )

type DebugHUDEntity = (DebugHUD, ShaderProgram, BufferResource)
type HUDEntity = (FontInfo, HUDType, Position3D)

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

hudProgramName :: ProgramName
hudProgramName = ProgramName "debug_hud"

loadFontMap :: IO FontMap
loadFontMap = do
  let fontFilePath = fontPath </> "Roboto-Regular.ttf"
  fontMap <- loadCharacters fontFilePath 48 textureUnit characters
  return fontMap

initDebugHUD :: IO (BufferResource, ShaderProgram)
initDebugHUD = do
  let vertexShader   = shaderPath </> "debug_hud.v.glsl"
      fragmentShader = shaderPath </> "debug_hud.f.glsl"
  -- load in shaders
  program <- createProgram [ ShaderInfo GL.VertexShader   vertexShader
                           , ShaderInfo GL.FragmentShader fragmentShader ]
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

  return
    ( BufferResource { _vertexBuffer   = Just vertsBuf
                     , _texCoordBuffer = Just uvBuf
                     , _normalBuffer   = Nothing
                     , _rgbCoordBuffer = Nothing
                     , _indexBuffer    = Nothing  }
    , program
    )

updatePosText :: Position3D -> FontInfo -> FontInfo
updatePosText (Position3D (L.V3 x y z)) fi =
  let posText = "(" ++ intercalate ", " (map (take 3 . show) [x, y, z]) ++ ")"
  in fi { _fText = "Pos: " ++ posText }

updateFpsText :: String -> FontInfo -> FontInfo
updateFpsText str fi = fi { _fText = "Fps: " ++ str }

updateFacingText :: Facing -> FontInfo -> FontInfo
updateFacingText (Facing dir) fi = fi { _fText = "Facing: " ++ show dir }

updateHUDText :: String -> Facing -> Position3D -> (FontInfo, HUDType) -> FontInfo
updateHUDText fps fac pos (fi, PositionTracker) = updatePosText    pos fi
updateHUDText fps fac pos (fi, FPSCounter     ) = updateFpsText    fps fi
updateHUDText fps fac pos (fi, PlayerFacing   ) = updateFacingText fac fi

stepDebugHUD :: ECS ()
stepDebugHUD = do
  -- get past time & current time
  (GlobalTime { _pastTime    = pTime
              , _currentTime = cTime }) <- getGlobalTime
  let fpsDt = take 4 $ show (1 / ((cTime - pTime) / 1000 ))
  -- get camera position
  cmapM_ $ \((_, (_, cPos)) :: CameraEntity) -> do
    cmapM_ $ \(Player facing) -> do
      cmap $ updateHUDText fpsDt facing cPos

drawDebugHUD :: RenderGlobals -> HUDEntity -> IO ()
drawDebugHUD globals (fontInfo, _, Position3D cPos) = do
  let dims               = _rgDims globals
      FontMap fontMap    = _rgFontMap globals
      ProgramMap progMap = _rgProgramMap globals
      (br, sProgram)     = progMap ! hudProgramName
      program            = _glProgram sProgram
      vpLocation         = getAttrib  sProgram "vertexPosition_screenSpace"
      uvLocation         = getAttrib  sProgram "vertexUV"
      sampLocation       = getUniform sProgram "fontTextureSampler"
      dimsLocation       = getUniform sProgram "dims"
      posBuffer          = _vertexBuffer   br
      uvBuffer           = _texCoordBuffer br
  -- clear the depth buffer
  GL.clear [GL.DepthBuffer]
  -- set current program to shaderProgram
  GL.currentProgram               $= Just program
  -- enable all attributes
  GL.vertexAttribArray vpLocation $= GL.Enabled
  GL.vertexAttribArray uvLocation $= GL.Enabled
  -- handle uniforms
  -- set "dims" to viewport dims
  (realToFrac <$> dims :: L.V2 Float) `U.asUniform` dimsLocation

  -- enable blending func
  GL.blend     $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  -- set "fontTextureSampler" uniform
  GL.activeTexture               $= textureUnit
  GL.uniform sampLocation        $= GL.Index1 (2 :: GL.GLint)

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

    -- replace UV buffer with new coords
    GL.bindBuffer GL.ArrayBuffer      $= uvBuffer
    GL.vertexAttribPointer uvLocation $=
      ( GL.ToFloat
      , GL.VertexArrayDescriptor 2 GL.Float 0 offset0 )
    -- -- draw indexed triangles
    GL.drawArrays GL.Triangles 0 6
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

-- drawDebugHUD :: DebugHUDEntity -> (L.V2 Int32) -> HUDType -> IO ()
-- drawDebugHUD (hud, sProgram, br) dims hudType = do
--   let (HUDInfo hudInfo) = _hudInfo hud
--       (FontMap fontMap) = _fontMap hud
--       fontInfo          = hudInfo ! hudType
--       program           = _glProgram sProgram
--       vpLocation        = getAttrib  sProgram "vertexPosition_screenSpace"
--       uvLocation        = getAttrib  sProgram "vertexUV"
--       sampLocation      = getUniform sProgram "fontTextureSampler"
--       dimsLocation      = getUniform sProgram "dims"
--       posBuffer         = _vertexBuffer   br
--       uvBuffer          = _texCoordBuffer br
--   -- clear the depth buffer
--   GL.clear [GL.DepthBuffer]
--   -- set current program to shaderProgram
--   GL.currentProgram               $= Just program
--   -- enable all attributes
--   GL.vertexAttribArray vpLocation $= GL.Enabled
--   GL.vertexAttribArray uvLocation $= GL.Enabled
--   -- handle uniforms
--   -- set "dims" to viewport dims
--   (realToFrac <$> dims :: L.V2 Float) `U.asUniform` dimsLocation
--
--   -- enable blending func
--   GL.blend     $= GL.Enabled
--   GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
--
--   -- set "fontTextureSampler" uniform
--   GL.activeTexture               $= textureUnit
--   GL.uniform sampLocation        $= GL.Index1 (2 :: GL.GLint)
--
--   foldM_ (\advAcc chr -> do
--     let character              = fontMap ! chr
--         scale                  = _fSize fontInfo
--         texture                = _charTexture character
--         GL.TextureSize2D sW sH = _textureSize texture
--         L.V2 bX bY             = _charBearing character
--     -- generate new buffer coordinates here
--         sX                     = realToFrac sW
--         sY                     = realToFrac sH
--         xPos                   = (_fxPos fontInfo + advAcc) + (bX * scale)
--         yPos                   = (_fyPos fontInfo) - ((sY - bY) * scale)
--         w                      = sX * scale
--         h                      = sY * scale
--     -- new VBO vertices
--         verts                  = [ L.V2 xPos       (yPos + h)   -- bottom left
--                                  , L.V2 xPos       yPos         -- top left
--                                  , L.V2 (xPos + w) yPos         -- top right
--                                  , L.V2 xPos       (yPos + h)   -- bottom left
--                                  , L.V2 (xPos + w) yPos         -- top right
--                                  , L.V2 (xPos + w) (yPos + h) ] -- bottom right
--
--     -- bind to current letter texture
--     GL.textureBinding GL.Texture2D $= (_textureId texture)
--
--     -- replace vertex buffer with new coords
--     GL.bindBuffer GL.ArrayBuffer      $= posBuffer
--     replaceBuffer (GL.DynamicDraw, GL.ArrayBuffer) verts
--     GL.vertexAttribPointer vpLocation $=
--       ( GL.ToFloat
--       , GL.VertexArrayDescriptor 2 GL.Float 0 offset0 )
--
--     -- replace UV buffer with new coords
--     GL.bindBuffer GL.ArrayBuffer      $= uvBuffer
--     GL.vertexAttribPointer uvLocation $=
--       ( GL.ToFloat
--       , GL.VertexArrayDescriptor 2 GL.Float 0 offset0 )
--     -- -- draw indexed triangles
--     GL.drawArrays GL.Triangles 0 6
--     -- accumulate advance offset (remember it's in 1/64 pixels, so divide it)
--     return $ advAcc + (((realToFrac $ _charAdvance character) / 64) * scale)
--     ) 0 $ _fText fontInfo
--
--   -- disable blending func
--   GL.blend     $= GL.Disabled
--
--   -- disable all attributes
--   GL.vertexAttribArray vpLocation $= GL.Disabled
--   GL.vertexAttribArray uvLocation $= GL.Disabled
--   -- unbind array buffer
--   GL.bindBuffer GL.ArrayBuffer $= Nothing
--   -- unset current program
--   GL.currentProgram            $= Nothing
--   return ()
