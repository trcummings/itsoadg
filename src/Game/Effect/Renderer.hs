{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Renderer where

import           SDL (($=))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Game.Types (VideoConfig(..))
import           Game.Effect.HasVideoConfig (HasVideoConfig(..))

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen  :: m ()

clearScreen' :: (HasVideoConfig m, MonadIO m) => m ()
clearScreen' = do
  -- (prog, attrib) <- vcGLResources <$> getVideoConfig
  liftIO $ GL.clearColor $= GL.Color4 1 1 1 1
  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ GL.viewport $= ( GL.Position 0 0
                          , GL.Size
                              (fromIntegral 640)
                              (fromIntegral 480) )

drawScreen' :: (HasVideoConfig m, MonadIO m) => m ()
drawScreen' = do
  cfg <- getVideoConfig
  let window            = vcWindow cfg
      (program, attrib) = vcGLResources cfg

  -- set current program
  liftIO $ GL.currentProgram $= Just program

  -- render vertext array
  liftIO $ GL.vertexAttribArray attrib $= GL.Enabled
  liftIO $ V.unsafeWith vertices $ \ptr ->
      liftIO $ GL.vertexAttribPointer attrib $=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
  liftIO $ GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
  liftIO $ GL.vertexAttribArray attrib $= GL.Disabled

  -- swap buffer
  SDL.glSwapWindow window

vertices :: V.Vector Float
vertices = V.fromList [  0.0,  0.8
                      , -0.8, -0.8
                      ,  0.8, -0.8
                      ]
