module Game.Util.Billboard where

import Graphics.Rendering.OpenGL
  ( TextureObject(..)
  , Size(..)
  , PixelData(..)
  , TextureTarget2D (Texture2D)
  , TextureFilter   (Linear', Nearest)
  , TextureFunction (Modulate)
  , PixelInternalFormat (RGBA')
  , textureFunction
  , build2DMipmaps
  , textureFilter
  , textureBinding
  , genObjectNames
  , ($=) )

renderBillboard :: TextureObject -> IO ()
renderBillboard tex = do
  return ()
-- renderCrosshair texs = do
--    Just crosshairTex <- HT.lookup texs "crosshair"
--    texture Texture2D $= Enabled
--    textureBinding Texture2D $= crosshairTex
--    unsafePreservingMatrix $ do
--       loadIdentity
--       translate (Vector3 304 224 (0::Float))
--       alphaFunc $= Just (Greater,0.1:: Float)
--       unsafeRenderPrimitive Quads $ do
--          texCoord (TexCoord2 0 (1 :: Float))
--          vertex   (Vertex2 0 (0 :: Float))
--          texCoord (TexCoord2 0 (0 :: Float))
--          vertex   (Vertex2 0 (32 :: Float))
--          texCoord (TexCoord2 1 (0 :: Float))
--          vertex   (Vertex2 32 (32 :: Float))
--          texCoord (TexCoord2 1 (1 :: Float))
--          vertex   (Vertex2 32 (0 :: Float))
--       alphaFunc $= Nothing
--    texture Texture2D $= Disabled
