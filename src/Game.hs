{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (main) where

import qualified SDL
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
import qualified Apecs as ECS            (System, runSystem)
import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)
import           Data.IORef

import           Game.World (World, initWorld)
import           Game.Types
  ( Env(..)
  , VideoConfig(..)
  , DebugMode(..)
  , RuntimeConfig(..)
  , EventQueue(..)
  , RunState(..)
  , GameState(..) )
import           Game.System.Init (initSystems)
import           Game.Util.Constants (initialSize)
import           Game.Util.TileMap (basicTilemap)

-- import           Game.Loop (mainLoop)

import           Game.Effect.Renderer     (Renderer(..), clearScreen', drawScreen')
-- import           Game.Effect.Event        (Event(..), prependAndGetEvents', setEvents')
import           Game.Effect.HasGameState     ( HasGameState(..)
                                              , getGameState'
                                              , setGameState' )
import           Game.Effect.HasVideoConfig   ( HasVideoConfig(..)
                                              , getVideoConfig' )
import           Game.Effect.HasRuntimeConfig ( HasRuntimeConfig
                                              , getRuntimeConfig' )

import           Game.Wrapper.SDLRenderer ( SDLRenderer(..)
                                          , presentRenderer'
                                          , clearRenderer' )
import           Game.Wrapper.SDLInput    (SDLInput(..), pollEvents')
import           Game.Wrapper.SDLTime     (SDLTime(..), nextTick')
import           Game.Wrapper.Apecs       (Apecs(..), runSystem', runGC')

newtype Game a = Game
  (ReaderT Env (ECS.System World) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadIO )

-- runGame :: Env -> Game a -> IO a
-- runGame env (Game m) = runReaderT m env

main :: IO ()
main = do
  SDL.initializeAll -- initialize all SDL systems
  TTF.initialize   -- initialize SDL.Font
  Mixer.openAudio Mixer.defaultAudio 256 -- initialize SDL.Mixer with 256 chunk size

  -- create window and renderer
  window <- SDL.createWindow
    "ITSOADG"
    SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let videoConfig = VideoConfig { vcWindow   = window
                                , vcRenderer = renderer }

  -- register joystick to receive events from it
  -- joysticks <- SDL.availableJoysticks
  -- mapM_ SDL.openJoystick joysticks

  -- initialize game
  world <- initWorld
  ECS.runSystem (initSystems renderer) world
  -- tileMap <- initTilemap basicTilemap world

  -- start loop
  debugMode <- newIORef DebugMode'DrawDebug
  gameState <- newIORef $ GameState { gsRunState   = RunState'Running
                                    , gsEventQueue = EventQueue []
                                    , gsTileMap    = basicTilemap }

  let runtimeConfig = RuntimeConfig { rcDebugMode = debugMode }

      env = Env { envVideoConfig   = videoConfig
                , envRuntimeConfig = runtimeConfig
                , envGameState     = gameState }
  -- start loop
  -- runGame mainLoop

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

-- effects
instance HasGameState Game where
  getGameState = getGameState'
  setGameState = setGameState'

instance HasRuntimeConfig Game where
  getRuntimeConfig = getRuntimeConfig'

instance HasVideoConfig Game where
  getVideoConfig = getVideoConfig'

-- instance HasEventQueue Game where
--   prependAndGetEvents = prependAndGetEvents'
--   setEvents = setEvents'

-- instance HasRunState Game where
--   getRunState = getRunState'

-- instance HasTilemap Game where
--   getTilemap = undefined

-- instance HasECS Game where
--   cmap   = undefined
--   cmapM  = undefined
--   cmapM_ = undefined

-- modules
instance Renderer Game where
  clearScreen = clearScreen'
  drawScreen  = drawScreen'
