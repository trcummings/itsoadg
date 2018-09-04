module Game.Effect.Renderer where

import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import           SDL (($=))
import           Apecs (exists, Proxy(..), cmapM_, getAll)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (maybe)
import           Data.Int (Int32)
import           Linear (V2(..))

import           Game.Types (VideoConfig(..))
import           Game.World.TH (ECS)

clearScreen :: ECS ()
clearScreen = withVC prepNextRender

drawScreen :: ECS ()
drawScreen = withVC swapBuffer

withVC :: (VideoConfig -> IO ()) -> ECS ()
withVC f = do
  vcs <- getAll :: ECS [VideoConfig]
  if (length vcs == 0)
  then return ()
  else do
    let (vc:_) = vcs
    cmapM_ $ \(vc :: VideoConfig) -> liftIO $ f vc


getWindowDims :: VideoConfig -> IO (V2 Int32)
getWindowDims vc = do
  (V2 width height) <- SDL.get $ SDL.windowSize $ _window vc
  return $ V2 (fromIntegral width :: Int32) (fromIntegral height :: Int32)

prepNextRender :: VideoConfig -> IO ()
prepNextRender vc = do
  -- clear background color to black
  GL.clearColor $= GL.Color4 0 0 0 0
  -- set depth function to sort by "Less" depth
  GL.depthFunc  $= Just GL.Less
  -- clear buffers
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  -- set viewport
  (V2 width height) <- getWindowDims vc
  GL.viewport $= ( GL.Position 0 0
                 , GL.Size width height )

swapBuffer :: VideoConfig -> IO ()
swapBuffer vc = SDL.glSwapWindow $ _window vc
