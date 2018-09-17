module Game.Util.FBO where
-- module Game.Util.FBO (FBO(..), makeFramebuffer) where
--
-- import qualified Graphics.Rendering.OpenGL as GL
-- import           SDL           (($=))
-- import           Foreign.Ptr   (nullPtr)
-- import           Control.Monad (when)
--
-- data FBO = FBO
--   { _frameBuffer :: GL.FramebufferObject
--   , _fbTexture   :: GL.TextureObject
--   , _size        :: GL.Size }
--   deriving Show
--
-- makeFramebuffer :: (GL.GLsizei, GL.GLsizei) -> IO FBO
-- makeFramebuffer (winW, winH) = do
--   -- create texture (color buffer)
--   fbTex <- GL.genObjectName
--   GL.textureBinding GL.Texture2D $= Just fbTex
--   -- specify texture filtering and wrapping
--   GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
--   GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
--   GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)
--   -- fill the texture with nothing, give OpenGL its specs
--   GL.texImage2D GL.Texture2D
--       GL.NoProxy
--       0
--       GL.RGB'
--       (GL.TextureSize2D winW winH)
--       0
--       (GL.PixelData GL.RGB GL.UnsignedByte nullPtr)
--   GL.textureBinding GL.Texture2D $= Nothing
--
--   -- create depth buffer.
--   depthRenderbuffer <- GL.genObjectName
--   GL.bindRenderbuffer GL.Renderbuffer $= depthRenderbuffer
--   let rbufSize = GL.RenderbufferSize winW winH
--   GL.renderbufferStorage GL.Renderbuffer GL.DepthComponent' rbufSize
--   GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
--
--   -- create an FBO and bind it
--   fbName <- GL.genObjectName
--   GL.bindFramebuffer GL.Framebuffer $= fbName
--   -- set framebuffer's depth buffer
--   GL.framebufferRenderbuffer GL.Framebuffer
--       GL.DepthAttachment GL.Renderbuffer depthRenderbuffer
--   -- set framebuffer's texture
--   GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0)
--                           GL.Texture2D fbTex 0
--
--   GL.drawBuffers $= [GL.FBOColorAttachment 0]
--   -- check that the frame buffer is complete
--   fbufStatus <- GL.get $ GL.framebufferStatus GL.Framebuffer
--   when (fbufStatus /= GL.Complete) $ do
--       error $ "frambufferStatus returned a value " ++
--               "other than 'Complete'. Status returned: '" ++
--               show fbufStatus ++ "'."
--   -- return our FBO
--   return $ FBO { _frameBuffer = fbName
--                , _fbTexture   = fbTex
--                , _size        = GL.Size winW winH }