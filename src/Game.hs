{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (main) where

import qualified SDL
import           SDL (($=))
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class  (MonadIO(..), liftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)
import           Data.IORef

import           Game.World (Env, World, initWorld)
import           Game.Types
  ( GameEnv(..)
  , VideoConfig(..)
  , Scene(..)
  , GameState(..) )
import           Game.Util.Constants (initialSize)
import           Game.Loop (mainLoop)
import           Game.Effect.Renderer         ( Renderer(..)
                                              , drawScreen'
                                              , clearScreen' )
import           Game.Effect.HasGameState     ( HasGameState(..)
                                              , getGameState'
                                              , setGameState' )
import           Game.Effect.HasVideoConfig   ( HasVideoConfig(..)
                                              , getVideoConfig' )
import           Game.Effect.HasECSWorld      ( HasECSWorld(..)
                                              , getECSWorld' )
import           Game.Effect.SceneManager     ( SceneManager(..)
                                              , getScene'
                                              , setScene'
                                              , getNextScene'
                                              , setNextScene' )
import           Game.Wrapper.SDLInput    (SDLInput(..), pollEvents')
import           Game.Wrapper.SDLTime     (SDLTime(..), nextTick')
import           Game.Wrapper.Apecs       ( Apecs(..)
                                          , runSystem'
                                          , runGC'
                                          , newEntity'
                                          , cmap'
                                          -- , qmap'
                                          , get'
                                          , set'
                                          , cmapM'
                                          , cmapM_'
                                          , getAll'
                                          , destroy'
                                          , modify'
                                          , exists' )

newtype Game a = Game
  (ReaderT Env IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadIO )

runGame :: Env -> Game m -> IO m
runGame env (Game m) = runReaderT m env

main :: IO ()
main = do
  -- initialize all SDL systems
  SDL.initializeAll
  -- initialize SDL.Font
  TTF.initialize
  -- initialize SDL.Mixer with 256 chunk size
  Mixer.openAudio Mixer.defaultAudio 256
  -- check for enabled linear texture filtering
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  -- create window and renderer
  window <- SDL.createWindow
    "Let Sleeping Gods Lie"
    SDL.defaultWindow { SDL.windowInitialSize = initialSize
                      , SDL.windowOpenGL      = Just SDL.defaultOpenGL }
  -- create OpenGL renderer context
  renderer       <- SDL.glCreateContext window
  let videoConfig = VideoConfig { _Window    = window
                                , _GLContext = renderer }

  -- register joystick to receive events from it
  -- joysticks <- SDL.availableJoysticks
  -- mapM_ SDL.openJoystick joysticks

  -- initialize ECS game world
  world <- initWorld

  -- initialize various env variables
  gameState <- newIORef $ GameState { _Scene     = Scene'Init
                                    , _NextScene = Scene'Title }

  let env = GameEnv { _VideoConfig = videoConfig
                    , _GameState   = gameState
                    , _World       = world }
  -- start loop
  runGame env mainLoop

  -- clean up on quit
  SDL.glDeleteContext renderer
  SDL.destroyWindow window
  Mixer.closeAudio
  Mixer.quit
  TTF.quit
  SDL.quit

-- wrappers
instance SDLInput Game where
  pollEvents = pollEvents'

instance SDLTime Game where
  nextTick = nextTick'

instance Apecs Game where
  runSystem = runSystem'
  runGC     = runGC'
  newEntity = newEntity'
  cmap      = cmap'
  cmapM     = cmapM'
  cmapM_    = cmapM_'
  -- qmap      = qmap'
  get       = get'
  set       = set'
  getAll    = getAll'
  destroy   = destroy'
  modify    = modify'
  exists    = exists'

-- effects
instance HasGameState Game where
  getGameState = getGameState'
  setGameState = setGameState'

instance SceneManager Game where
  getScene     = getScene'
  setScene     = setScene'
  getNextScene = getNextScene'
  setNextScene = setNextScene'

instance HasVideoConfig Game where
  getVideoConfig = getVideoConfig'

instance HasECSWorld Game where
  getECSWorld = getECSWorld'

-- -- modules
instance Renderer Game where
  clearScreen = clearScreen'
  drawScreen  = drawScreen'
