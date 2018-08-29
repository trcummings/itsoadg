-- {-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (main) where

import qualified SDL
import           SDL (($=))
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
-- import qualified Graphics.Rendering.OpenGL as GL
-- import qualified Data.ByteString as BS
-- import           Foreign.C.Types
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class  (MonadIO(..), liftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, runReaderT)
import           Data.IORef
-- import           System.Exit (exitFailure)
-- import           System.IO

import           Game.World (Env, World, initWorld)
import           Game.Types
  ( GameEnv(..)
  , VideoConfig(..)
  -- , DebugMode(..)
  -- , RuntimeConfig(..)
  -- , EventQueue(..)
  , Scene(..)
  , GameState(..) )
import           Game.Util.Constants (initialSize)
-- import           Game.Util.TileMap (basicTilemap)
-- import           Game.Util.Shaders (makeShader, makeProgram)
import           Game.Loop (mainLoop)
import           Game.Effect.Renderer         ( Renderer(..)
                                              , drawScreen'
                                              , clearScreen' )
import           Game.Effect.HasGameState     ( HasGameState(..)
                                              , getGameState'
                                              , setGameState' )
import           Game.Effect.HasVideoConfig   ( HasVideoConfig(..)
                                              , getVideoConfig' )
-- import           Game.Effect.HasRuntimeConfig ( HasRuntimeConfig(..)
--                                               , getRuntimeConfig' )
import           Game.Effect.HasECSWorld      ( HasECSWorld(..)
                                              , getECSWorld' )
import           Game.Effect.SceneManager     ( SceneManager(..)
                                              , getScene'
                                              , setScene'
                                              , getNextScene'
                                              , setNextScene' )
-- import           Game.Effect.HasEventQueue    ( HasEventQueue(..)
--                                               , getEvents'
--                                               , prependAndGetEvents'
--                                               , setEvents' )
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

-- type GLResources = (GL.Program, GL.AttribLocation)

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
                      , SDL.windowOpenGL = Just SDL.defaultOpenGL }
  -- create OpenGL renderer context
  renderer       <- SDL.glCreateContext window
  -- (prog, attrib) <- initResources
  let videoConfig = VideoConfig { vcWindow      = window
                                , vcGLContext   = renderer }
                                -- , vcGLResources = (prog, attrib) }

  -- register joystick to receive events from it
  -- joysticks <- SDL.availableJoysticks
  -- mapM_ SDL.openJoystick joysticks

  -- initialize ECS game world
  world <- initWorld

  -- initialize various env variables
  -- debugMode <- newIORef DebugMode'DrawDebug
  gameState <- newIORef $ GameState { gsScene      = Scene'Init
                                    , gsNextScene  = Scene'Title }
                                    -- , gsEventQueue = EventQueue []
                                    -- , gsTileMap    = basicTilemap }

  -- let runtimeConfig = RuntimeConfig { rcDebugMode = debugMode }
  let env = GameEnv { envVideoConfig   = videoConfig
                    -- , envRuntimeConfig = runtimeConfig
                    , envGameState     = gameState
                    , envECSWorld      = world }
  -- start loop
  runGame env mainLoop

  -- clean up on quit
  SDL.glDeleteContext renderer
  SDL.destroyWindow window
  Mixer.closeAudio
  Mixer.quit
  TTF.quit
  SDL.quit

-- initResources :: IO (GL.Program, GL.AttribLocation)
-- initResources = do
--   vs <- makeShader GL.VertexShader   vsSource
--   fs <- makeShader GL.FragmentShader fsSource
--   program <- makeProgram [vs, fs] [("coord2d", GL.AttribLocation 0)]
--   GL.currentProgram $= Just program
--   return (program, GL.AttribLocation 0)

-- -- GLSL code for the vertex shader
-- vsSource :: BS.ByteString
-- vsSource = BS.intercalate "\n"
--            [
--             "attribute vec2 coord2d; "
--            , ""
--            , "void main(void) { "
--            , " gl_Position = vec4(coord2d, 0.0, 1.0); "
--            , "}"
--            ]
--
-- -- GLSL code for the fragment shader
-- fsSource :: BS.ByteString
-- fsSource = BS.intercalate "\n"
--            [
--             ""
--            , "#version 120"
--            , "void main(void) {"
--            , "gl_FragColor = vec4((gl_FragCoord.x/640), (gl_FragCoord.y/480), 0, 1);"
--            , "}"
--            ]

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

-- instance HasRuntimeConfig Game where
--   getRuntimeConfig = getRuntimeConfig'

instance HasVideoConfig Game where
  getVideoConfig = getVideoConfig'

instance HasECSWorld Game where
  getECSWorld = getECSWorld'

-- instance HasEventQueue Game where
--   prependAndGetEvents = prependAndGetEvents'
--   getEvents = getEvents'
--   setEvents = setEvents'

-- -- modules
instance Renderer Game where
  clearScreen = clearScreen'
  drawScreen  = drawScreen'
