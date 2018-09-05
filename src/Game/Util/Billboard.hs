module Game.Util.Billboard where

import Graphics.Rendering.OpenGL
import Linear

import Game.Types (Position3D(..))

renderBillboard :: Position3D -> Maybe TextureObject -> IO ()
renderBillboard (Position3D (V3 x y z)) tex = do
   texture Texture2D $= Enabled
   textureBinding Texture2D $= tex
   unsafePreservingMatrix $ do
      loadIdentity
      translate (Vector3 x y z)
      alphaFunc $= Just (Greater, 0.1 :: Float)
      unsafeRenderPrimitive Quads $ do
         texCoord (TexCoord2 0 (1 :: Float))
         vertex   (Vertex2 0 (0 :: Float))
         texCoord (TexCoord2 0 (0 :: Float))
         vertex   (Vertex2 0 (32 :: Float))
         texCoord (TexCoord2 1 (0 :: Float))
         vertex   (Vertex2 32 (32 :: Float))
         texCoord (TexCoord2 1 (1 :: Float))
         vertex   (Vertex2 32 (0 :: Float))
      alphaFunc $= Nothing
   texture Texture2D $= Disabled
