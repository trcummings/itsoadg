module Game.Audio where

import qualified SDL.Mixer as Mixer
  ( Channel
  , playOn
  , halt
  , play
  , playing )
import           Data.Map ((!), (!?))
import qualified Data.Map as Map (filter, elems, insert)
import           Data.Maybe (catMaybes)
import           Apecs (Entity, cmap, cmapM_, set)
import           Control.Monad (filterM, foldM)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Game.Types (SoundBank(..), SFX'Key, Audio'Command(..))
import           Game.World (System')

dispatchToAudioInbox :: (Entity, SFX'Key, Audio'Command) -> System' ()
dispatchToAudioInbox cmd = cmap $ \(SoundBank sb cm ibx) -> SoundBank sb cm (ibx ++ [cmd])

cullNonPlayingEntities :: System' ()
cullNonPlayingEntities = cmapM_ $ \(sBank@(SoundBank _ _ _), e) -> do
  -- get list of currently playing channels
  let channels = map snd (Map.elems (channelMap sBank))
  playingChannels <- filterM (liftIO . Mixer.playing) channels
  -- set new channel map to entity keys with only still-playing chunks
  let isPlaying = flip elem playingChannels . snd
  set e (sBank { channelMap = Map.filter isPlaying (channelMap sBank) } )

runCommand :: SoundBank -> (Entity, SFX'Key, Audio'Command) -> IO SoundBank
runCommand sb (e, key, command) = do
  let cm       = channelMap sb
  -- get the entity's current chunk & channel, if any
      chResult = cm !? e
  -- get the desired chunk to command
      chunk    = (bank sb) ! key
  case chResult of
    -- when the channel is currently occupied
    Just (pastKey, channel) -> do
      if (key == pastKey)
      then case command of
        Audio'Halt          -> do
          liftIO $ Mixer.halt channel
          return $ sb { channelMap = cm }
        Audio'PlayOrSustain ->
          return $ sb { channelMap = cm }
      else case command of
        Audio'Halt          ->
          return $ sb { channelMap = cm }
        Audio'PlayOrSustain -> do
          liftIO $ Mixer.halt channel
          liftIO $ Mixer.play chunk
          return $ sb { channelMap = Map.insert e (key, channel) cm }
    Nothing                    -> do
      case command of
        Audio'Halt          ->
          return $ sb { channelMap = cm }
        Audio'PlayOrSustain -> do
          channel' <- liftIO $ Mixer.playOn (-1) 1 chunk
          return $ sb { channelMap =  Map.insert e (key, channel') cm }

registerAndPlayCommands :: System' ()
registerAndPlayCommands = cmapM_ $ \(sBank@(SoundBank _ _ _), sE) -> do
  sBank' <- liftIO $ foldM runCommand sBank (inbox sBank)
  -- clear out the commands
  set sE (sBank' { inbox = [] })

stepAudioQueue :: System' ()
stepAudioQueue = do
  cullNonPlayingEntities
  registerAndPlayCommands
