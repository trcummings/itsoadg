{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.System.OptionMenu where

import qualified SDL
import           Apecs (Not(..), proxy)

import           Data.Map ((!), keys)
import           Data.List (find, findIndex)
import           Control.Lens ((&), (%~), element)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           KeyState (isPressed)

import           Game.Effect.SceneManager (SceneManager, setNextScene)
import           Game.Effect.Input (Input, getInputs)
import           Game.Wrapper.Apecs (Apecs(..))

import           Game.Types
  ( OptionList(..)
  , Option(..)
  , ActiveOptionList(..)
  , HasOptionMenuEvent(..)
  , OptionMenuCommand(..)
  , PlayerInput(..)
  , Scene(..) )

oIdAction :: (SceneManager m, MonadIO m) => String -> m ()
oIdAction oId = do
  liftIO $ putStrLn $ "Running " ++ oId ++ "!"
  case oId of
    "ToScene_Play"       -> setNextScene Scene'Play
    -- "ToScene_SelectFile" -> return ()
    -- "ToScene_Options"    -> return ()
    "ToScene_Quit"       -> setNextScene Scene'Quit
    _                    -> return ()

titleOptions :: [Option]
titleOptions = [  Option { oId      = "ToScene_Play"
                         , text     = "New Game"
                         , selected = True }
                -- , Option { oId      = "ToScene_SelectFile"
                --          , text     = "Load Game"
                --          , selected = False }
                -- , Option { oId      = "ToScene_Options"
                --          , text     = "Options"
                --          , selected = False }
                , Option { oId      = "ToScene_Quit"
                         , text     = "Quit Game"
                         , selected = False }
                ]

type OptionMenu = ( Maybe ActiveOptionList
                  , Maybe HasOptionMenuEvent
                  , OptionList )

initOptionsMenu :: (Apecs m, MonadIO m) => m ()
initOptionsMenu = do
  -- options menu
  newEntity (
      -- Position $ V2 5 5
      OptionList titleOptions
    , ActiveOptionList )
  return ()

cleanUpOptionsMenu :: Apecs m => m ()
cleanUpOptionsMenu = do
  -- delete the options menu
  cmapM_ $ \(_ :: OptionMenu, ety) ->
    destroy ety (proxy :: OptionMenu)

updateOption :: OptionMenuCommand -> OptionList -> OptionList
updateOption SelectOption ol = ol
updateOption move (OptionList options) =
  let op      = if move == MoveUp then (-) else (+)
      idx     = findIndex selected options
      -- next index in cyclical list
      nextIdx = flip mod (length options) <$> (flip op 1 <$> idx)
  in case (idx, nextIdx) of
      (Nothing, _) -> OptionList options
      (_, Nothing) -> OptionList options
      (Just idx1, Just idx2) ->
          OptionList
        $ options
        & element idx1 %~ (\opt -> opt { selected = not $ selected opt })
        & element idx2 %~ (\opt -> opt { selected = not $ selected opt })

stepOptionMenu :: ( Apecs m
                  , Input m
                  , SceneManager m
                  , MonadIO m
                  ) => m ()
stepOptionMenu = do
  -- when up or down key pressed, shift the selected option up or down
  -- when enter key pressed, use the selected oId to an event fn
  (PlayerInput { inputs = m }) <- getInputs
  let upPress    = isPressed $ m ! SDL.KeycodeW
      downPress  = isPressed $ m ! SDL.KeycodeS
      enterPress = isPressed $ m ! SDL.KeycodeReturn

  -- get all active option menus
  -- give them HasEvent component based on button presses
  -- (if simultaneous privilege enter presses, then down, then up)
  cmap $ \( ActiveOptionList
          , o :: OptionList
          , _ :: Not HasOptionMenuEvent ) ->
    if enterPress
    then Left (o, HasOptionMenuEvent SelectOption)
    else if downPress
         then Left (o, HasOptionMenuEvent MoveDown)
         else if upPress
              then Left (o, HasOptionMenuEvent MoveUp)
              else Right o

  -- handle option menu events
  cmapM_ $ \( ActiveOptionList
            , HasOptionMenuEvent command
            , OptionList options
            , ety ) -> do
    case command of
      -- find the selected oId, run its oIdAction, remove its event
      SelectOption -> do
        case oId <$> find selected options of
          Just selectedId -> oIdAction selectedId
          Nothing         -> return ()
        destroy ety (proxy :: Not HasOptionMenuEvent)
      -- otherwise, update the option, remove its event
      _            -> do
        modify  ety $ updateOption command
        destroy ety (proxy :: HasOptionMenuEvent)


renderOptionMenu :: (Apecs m, MonadIO m) => m ()
renderOptionMenu = do
  -- renderer  <- vcRenderer <$> getVideoConfig
  -- cmapM_ $ \(Font font) -> do
  --   -- render title
  --   liftIO $ renderText renderer font (V2 2 3) "Let Sleeping Gods Lie"
  --   --render options menu
  --   cmapM_ $ \(OptionList options, Position (V2 px py)) -> do
  --     -- render each option descending vertically
  --     mapM_ (\(option, yMod) -> do
  --         let newY = py + Unit yMod
  --         liftIO $ renderText renderer font (V2 px newY) (text option)
  --         -- draw arrow next to selected option
  --         when (selected option) $ do
  --           liftIO $ renderText renderer font (V2 (px - Unit 1) newY) ">"
  --       ) (zip options ( [x | x <- [0.0, 1.0..(fromIntegral $ length options)] ] ))
  --     return ()
  return ()
