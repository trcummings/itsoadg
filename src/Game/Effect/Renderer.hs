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

initScreen :: ECS ()
initScreen = withVC initRenderer

initRenderer :: VideoConfig -> IO ()
initRenderer vc = do
  -- clear background color to blue
  GL.clearColor $= GL.Color4 0 0 0.4 0
  -- enable clearing of the depth buffer
  GL.clearDepth $= 1
  -- set depth function to sort by "Less" depth
  -- allows for 3D depth testing & tells OpenGL how to deal with overlap
  -- which accepts the fragment if its closer to the camera than the
  -- former one
  GL.depthFunc  $= Just GL.Less
  -- set backface culling
  GL.cullFace   $= Just GL.Back
  -- set viewport
  setViewportToWindow vc

prepNextRender :: VideoConfig -> IO ()
prepNextRender vc = do
  -- clear background color to blue
  GL.clearColor $= GL.Color4 0 0 0.4 0
  -- clear buffers
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  -- set viewport
  setViewportToWindow vc

-- helpers
swapBuffer :: VideoConfig -> IO ()
swapBuffer vc = SDL.glSwapWindow $ _window vc

withVC :: (VideoConfig -> IO ()) -> ECS ()
withVC f = do
  vcs <- getAll :: ECS [VideoConfig]
  if (length vcs == 0)
  then return ()
  else do
    let (vc:_) = vcs
    cmapM_ $ \(vc :: VideoConfig) -> liftIO $ f vc

setViewportToWindow :: VideoConfig -> IO ()
setViewportToWindow vc = do
  (V2 width height) <- getWindowDims vc
  GL.viewport $= ( GL.Position 0 0
                 , GL.Size width height )

getWindowDims :: VideoConfig -> IO (V2 Int32)
getWindowDims vc = do
  (V2 width height) <- SDL.get $ SDL.windowSize $ _window vc
  return $ V2 (fromIntegral width :: Int32) (fromIntegral height :: Int32)
