module Game.Util.GLError where

import qualified Graphics.Rendering.OpenGL as GL
import           Control.Monad.IO.Class (liftIO)

printGLErrors :: String -> IO ()
printGLErrors whereAt = do
  errors <- GL.errors
  liftIO $ putStrLn $ "at " ++ whereAt ++ ": " ++ show errors
