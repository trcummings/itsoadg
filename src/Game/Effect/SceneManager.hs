module Game.Effect.SceneManager where

import Control.Monad (when)

data Scene =
    Scene'Title
  | Scene'FileSelect
  | Scene'Quit
  deriving (Eq, Show)

class SceneManager m where
  toScene :: Scene -> m ()

stepScene :: Scene -> Scene -> IO ()
stepScene scene nextScene = do
  when (nextScene /= scene) $ do
    return ()
