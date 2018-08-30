{-# LANGUAGE FlexibleContexts #-}

module Game.Effect.Renderer where

import           SDL (($=))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import           Data.Int (Int32)
import           Linear (V2(..))
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Game.Types (VideoConfig(..))
import           Game.Effect.HasVideoConfig (HasVideoConfig(..))

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen  :: m ()

clearScreen' :: (HasVideoConfig m, MonadIO m) => m ()
clearScreen' = do
  window <- vcWindow <$> getVideoConfig
  -- clear background color to black
  liftIO $ GL.clearColor $= GL.Color4 0 0 0 0
  -- set depth function to sort by "Less" depth
  liftIO $ GL.depthFunc  $= Just GL.Less
  -- clear buffers
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  -- set viewport
  (V2 width height) <- SDL.get $ SDL.windowSize window
  liftIO $ GL.viewport $= ( GL.Position 0 0
                          , GL.Size (fromIntegral width  :: Int32)
                                    (fromIntegral height :: Int32) )

drawScreen' :: (HasVideoConfig m, MonadIO m) => m ()
drawScreen' = do
  window <- vcWindow <$> getVideoConfig
  -- swap buffer
  SDL.glSwapWindow window
