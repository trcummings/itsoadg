{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (main) where

import qualified SDL
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
import qualified Apecs as ECS            (System, runSystem)
import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State     (MonadState , StateT , evalStateT)

import           Game.World (World, initWorld)
import           Game.Types (SDLConfig(..), EventQueue(..))
import           Game.FlowMeter (stepFlowMeter)
import           Game.Init (initSystems)
import           Game.Constants (initialSize)
import           Game.Loop (mainLoop)
import           Game.Effect.Renderer     (Renderer(..), clearScreen', drawScreen')
import           Game.Effect.Event        (Event(..))
import           Game.Wrapper.SDLRenderer (SDLRenderer(..), presentRenderer', clearRenderer')
import           Game.Wrapper.SDLInput    (SDLInput(..), pollEvents')
import           Game.Wrapper.SDLTime     (SDLTime(..), nextTick')
import           Game.Wrapper.Apecs       (Apecs(..), runSystem', runGC')

newtype Game a = Game
  (ReaderT SDLConfig (StateT EventQueue IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SDLConfig
    , MonadState EventQueue
    , MonadIO )

runGame :: SDLConfig -> EventQueue -> Game a -> IO a
runGame sdlConfig eq (Game m) = evalStateT (runReaderT m sdlConfig) eq

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
  let sdlConfig = SDLConfig { sdlWindow = window, sdlRenderer = renderer }

  -- register joystick to receive events from it
  joysticks <- SDL.availableJoysticks
  mapM_ SDL.openJoystick joysticks

  -- initialize Apecs world & add entities
  world <- initWorld
  ECS.runSystem (initSystems renderer) world

  -- start loop
  runGame sdlConfig (EventQueue []) (mainLoop world)
  -- mainLoop window renderer world

  -- clean up on quit
  SDL.destroyWindow window
  Mixer.closeAudio
  Mixer.quit
  TTF.quit
  SDL.quit

instance Renderer Game where
  clearScreen = clearScreen'
  drawScreen  = drawScreen'

instance Event Game where

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
