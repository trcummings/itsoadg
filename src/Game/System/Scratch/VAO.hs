module Game.System.Scratch.VAO where

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.GL.Core33     (glDeleteVertexArrays)
import           SDL                    (($=))
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Foreign.Marshal.Utils  (with)
import           Unsafe.Coerce          (unsafeCoerce)
import           Apecs
  ( Proxy(..)
  , destroy
  , newEntity
  , cmap
  , cmapM_ )

import           Game.World.TH (ECS)
import           Game.Types    (VAO(..))

initVAO :: ECS ()
initVAO = do
  -- create VAO & VAO entity
  [vao] <- GL.genObjectNames 1
  GL.bindVertexArrayObject $= Just vao
  void $ newEntity $ VAO vao

cleanUpVAO :: ECS ()
cleanUpVAO = do
  -- unbind the VAO & destroy the entity
  cmapM_ $ \(VAO vao, ety) -> do
    liftIO $ deleteVAO vao
    liftIO $ GL.bindVertexArrayObject $= Nothing
    destroy ety (Proxy :: Proxy VAO)

deleteVAO :: GL.VertexArrayObject -> IO ()
deleteVAO vao = do
  with (vaoID vao) $ glDeleteVertexArrays 1
    where vaoID = unsafeCoerce :: GL.VertexArrayObject -> GL.GLuint
