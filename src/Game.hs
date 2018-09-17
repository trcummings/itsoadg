module Game (main) where

import qualified SDL
import qualified SDL.Mixer as Mixer
import           Apecs              (runSystem)

import           Game.World.TH      (initWorld)
import           Game.Loop          (mainLoop)

main :: IO ()
main = do
  -- initialize all SDL systems
  SDL.initializeAll
  -- initialize SDL.Mixer with 256 chunk size
  Mixer.openAudio Mixer.defaultAudio 256
  -- start game
  initWorld >>= runSystem mainLoop
  -- clean up SDL systems
  Mixer.closeAudio
  Mixer.quit
  SDL.quit
