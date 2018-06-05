module Game.Audio where

import qualified SDL.Mixer as Mixer (play)
import           Data.Map ((!))
import           Apecs (cmap, cmapM_, set)
import           Control.Monad.IO.Class (liftIO)

import           Game.Types (SoundBank(..), SFX'Key)
import           Game.World (System')

dispatchToAudioInbox :: SFX'Key -> System' ()
dispatchToAudioInbox key = cmap $ \(SoundBank sb ibx) -> SoundBank sb (ibx ++ [key])

stepAudioQueue :: System' ()
stepAudioQueue = do
  cmapM_ $ \(SoundBank sb ibx, e) -> do
    mapM_ (\key -> do
              let chunk = sb ! key
              liftIO $ Mixer.play chunk ) ibx
    set e (SoundBank sb [])
