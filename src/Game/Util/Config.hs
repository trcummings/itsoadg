{-# LANGUAGE OverloadedStrings     #-}

module Game.Util.Config where

import qualified SDL
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
import           SDL (($=))
import           Control.Monad (when)

import           Game.Types.Env (VideoConfig(..))
import           Game.Util.Constants (initialSize)

initConfig :: IO VideoConfig
initConfig = do
    -- initialize all SDL systems
    SDL.initializeAll
    -- initialize SDL.Font
    TTF.initialize
    -- initialize SDL.Mixer with 256 chunk size
    Mixer.openAudio Mixer.defaultAudio 256
    -- check for enabled linear texture filtering
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do
      renderQuality <- SDL.get SDL.HintRenderScaleQuality
      when (renderQuality /= SDL.ScaleLinear) $
        putStrLn "Warning: Linear texture filtering not enabled!"
    -- register joystick to receive events from it
    -- joysticks <- SDL.availableJoysticks
    -- mapM_ SDL.openJoystick joysticks
    -- create window and renderer
    window <- SDL.createWindow
      "Let Sleeping Gods Lie"
      SDL.defaultWindow { SDL.windowInitialSize = initialSize
                        , SDL.windowOpenGL      = Just SDL.defaultOpenGL }
    -- create OpenGL renderer context
    glContext <- SDL.glCreateContext window

    -- return
    return $ VideoConfig { _window    = window
                         , _glContext = glContext }

cleanUpConfig :: VideoConfig -> IO ()
cleanUpConfig vc = do
  SDL.glDeleteContext $ _glContext vc
  SDL.destroyWindow   $ _window    vc
  Mixer.closeAudio
  Mixer.quit
  TTF.quit
  SDL.quit
