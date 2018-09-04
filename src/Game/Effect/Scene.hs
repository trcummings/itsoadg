module Game.Effect.Scene where

import           Apecs                  (get, set, global)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)

import qualified Game.Scene.Init  as Init  (initialize, step, cleanUp, render)
import qualified Game.Scene.Title as Title (initialize, step, cleanUp, render)
import qualified Game.Scene.Play  as Play  (initialize, step, cleanUp, render)
import qualified Game.Scene.Quit  as Quit  (initialize, step, cleanUp, render)
import           Game.Types                (SceneControl(..), Scene(..))
import           Game.World.TH             (ECS)

instance Show Scene where
  show Scene'Init  = "Init"
  show Scene'Title = "Title"
  show Scene'Play  = "Play"
  show Scene'Quit  = "Quit"

data SceneAction =
    Initialize
  | Step
  | Render
  | CleanUp
  deriving Show

currentSceneAction :: SceneAction -> ECS ()
currentSceneAction action = do
  sc <- get global :: ECS SceneControl
  runSceneAction action $ _scene sc

transitionScene :: Scene -> Scene -> ECS ()
transitionScene Scene'Init  Scene'Title =  (runSceneAction CleanUp Scene'Init)
                                        >> (runSceneAction Initialize Scene'Title)

transitionScene Scene'Title Scene'Quit  =  (runSceneAction CleanUp Scene'Title)
                                        >> (runSceneAction Initialize Scene'Quit)

transitionScene Scene'Title Scene'Play  =  (runSceneAction CleanUp Scene'Title)
                                        >> (runSceneAction Initialize Scene'Play)

transitionScene Scene'Play  Scene'Quit  =  (runSceneAction CleanUp Scene'Play)
                                        >> (runSceneAction Initialize Scene'Quit)

transitionScene s1          s2          = do
  error $ "Game.Effect.Scene: Scene transition error. "
       ++ show s1
       ++ " -> "
       ++ show s2
       ++ " is an illegal scene transition!"


runSceneAction :: SceneAction -> Scene -> ECS ()
runSceneAction action scene = do
  liftIO $ putStrLn $ show action ++ " scene " ++ show scene
  case action of
    Initialize -> case scene of Scene'Init  -> Init.initialize
                                Scene'Title -> Title.initialize
                                Scene'Play  -> Play.initialize
                                Scene'Quit  -> Quit.initialize

    Step       -> case scene of Scene'Init  -> Init.step
                                Scene'Title -> Title.step
                                Scene'Play  -> Play.step
                                Scene'Quit  -> Quit.step

    Render     -> case scene of Scene'Init  -> Init.render
                                Scene'Title -> Title.render
                                Scene'Play  -> Play.render
                                Scene'Quit  -> Quit.render

    CleanUp    -> case scene of Scene'Init  -> Init.cleanUp
                                Scene'Title -> Title.cleanUp
                                Scene'Play  -> Play.cleanUp
                                Scene'Quit  -> Quit.cleanUp


stepSceneControl :: ECS ()
stepSceneControl = do
  SceneControl { _scene     = s
               , _nextScene = ns } <- get global :: ECS SceneControl
  when (ns /= s) $ do
    -- run cleanUp/init function for current/next scene
    transitionScene s ns
    -- set current scene to next scene before next loop
    set global ( SceneControl { _scene     = ns
                              , _nextScene = ns } )

ifNotQuitting :: ECS () -> ECS ()
ifNotQuitting f = do
  sc <- get global :: ECS SceneControl
  case _scene sc of Scene'Quit  -> return ()
                    _           -> f
