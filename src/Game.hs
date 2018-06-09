{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game (main) where

import qualified SDL
import qualified SDL.Font  as TTF
import qualified SDL.Mixer as Mixer
import           Apecs (runSystem, runGC)

import           Game.World (System', World, initWorld)
import           Game.FlowMeter (stepFlowMeter)
import           Game.Init (initSystems)
import           Game.Constants (initialSize)
import           Game.Loop (mainLoop)


main :: IO ()
main = do
  SDL.initializeAll -- initialize all SDL systems
  TTF.initialize   -- initialize SDL.Font
  Mixer.openAudio Mixer.defaultAudio 256 -- initialize SDL.Mixer with 256 chunk size

  -- create window and renderer
  window <- SDL.createWindow "ITSOADG" SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- register joystick to receive events from it
  joysticks <- SDL.availableJoysticks
  mapM_ SDL.openJoystick joysticks

  -- initialize Apecs world & add entities
  world <- initWorld
  runSystem (initSystems renderer) world

  -- start loop
  mainLoop window renderer world

  -- clean up on quit
  SDL.destroyWindow window
  Mixer.closeAudio
  Mixer.quit
  TTF.quit
  SDL.quit
