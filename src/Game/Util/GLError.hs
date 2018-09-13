module Game.Util.GLError where

import qualified Graphics.Rendering.OpenGL as GL
import           Control.Monad (when)

printGLErrors :: String -> IO ()
printGLErrors whereAt = do
  errors <- GL.errors
  when (length errors > 0) $ do
    putStrLn $ "at " ++ whereAt ++ ": " ++ show errors
