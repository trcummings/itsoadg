{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (main) where

import qualified SDL
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
-- import qualified Apecs as ECS            (System(..), runSystem, unSystem)
import           Control.Monad.IO.Class  (MonadIO(..), liftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)
import           Data.IORef

import           Game.World (Env, World, initWorld)
import           Game.Types
  ( GameEnv(..)
  , VideoConfig(..)
  , DebugMode(..)
  , RuntimeConfig(..)
  , EventQueue(..)
  , Scene(..)
  , GameState(..) )
import           Game.System.Init (initSystems)
import           Game.Util.Constants (initialSize)
import           Game.Util.TileMap (basicTilemap)

import           Game.Loop (mainLoop)

import           Game.Effect.Renderer         ( Renderer(..)
                                              , clearScreen'
                                              , drawScreen' )
import           Game.Effect.HasGameState     ( HasGameState(..)
                                              , getGameState'
                                              , setGameState' )
import           Game.Effect.HasVideoConfig   ( HasVideoConfig(..)
                                              , getVideoConfig' )
import           Game.Effect.HasRuntimeConfig ( HasRuntimeConfig(..)
                                              , getRuntimeConfig' )
import           Game.Effect.HasECSWorld      ( HasECSWorld(..)
                                              , getECSWorld' )
import           Game.Effect.SceneManager     ( SceneManager(..)
                                              , getScene'
                                              , setScene'
                                              , getNextScene'
                                              , setNextScene' )
import           Game.Effect.HasEventQueue    ( HasEventQueue(..)
                                              , getEvents'
                                              , prependAndGetEvents'
                                              , setEvents' )

import           Game.Wrapper.SDLRenderer ( SDLRenderer(..)
                                          , presentRenderer'
                                          , clearRenderer' )
import           Game.Wrapper.SDLInput    (SDLInput(..), pollEvents')
import           Game.Wrapper.SDLTime     (SDLTime(..), nextTick')
import           Game.Wrapper.Apecs       ( Apecs(..)
                                          , runSystem'
                                          , runGC'
                                          , newEntity'
                                          , cmap'
                                          , qmap'
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
  SDL.initializeAll -- initialize all SDL systems
  TTF.initialize   -- initialize SDL.Font
  Mixer.openAudio Mixer.defaultAudio 256 -- initialize SDL.Mixer with 256 chunk size

  -- create window and renderer
  window <- SDL.createWindow
    "Let Sleeping Gods Lie"
    SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let videoConfig = VideoConfig { vcWindow   = window
                                , vcRenderer = renderer }

  -- register joystick to receive events from it
  -- joysticks <- SDL.availableJoysticks
  -- mapM_ SDL.openJoystick joysticks

  -- initialize game
  world <- initWorld
  -- ECS.runSystem (initSystems renderer) world
  -- tileMap <- initTilemap basicTilemap world

  debugMode <- newIORef DebugMode'DrawDebug
  gameState <- newIORef $ GameState { gsScene      = Scene'Init
                                    , gsNextScene  = Scene'Title
                                    , gsEventQueue = EventQueue []
                                    , gsTileMap    = basicTilemap }

  let runtimeConfig = RuntimeConfig { rcDebugMode = debugMode }
      env = GameEnv { envVideoConfig   = videoConfig
                    , envRuntimeConfig = runtimeConfig
                    , envGameState     = gameState
                    , envECSWorld      = world }
  -- start loop
  runGame env mainLoop

  -- clean up on quit
  SDL.destroyWindow window
  Mixer.closeAudio
  Mixer.quit
  TTF.quit
  SDL.quit

-- wrappers
instance SDLRenderer Game where
  presentRenderer = presentRenderer'
  clearRenderer   = clearRenderer'

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
  qmap      = qmap'
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

instance HasRuntimeConfig Game where
  getRuntimeConfig = getRuntimeConfig'

instance HasVideoConfig Game where
  getVideoConfig = getVideoConfig'

instance HasECSWorld Game where
  getECSWorld = getECSWorld'

instance HasEventQueue Game where
  prependAndGetEvents = prependAndGetEvents'
  getEvents = getEvents'
  setEvents = setEvents'

-- modules
instance Renderer Game where
  clearScreen = clearScreen'
  drawScreen  = drawScreen'
