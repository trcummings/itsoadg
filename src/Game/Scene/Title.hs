{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Scene.Title where

import qualified SDL
import           SDL.Font  as TTF (free, load, blended)
import           Apecs (proxy)
import           Linear (V4(..), V2(..))
import qualified Data.Map as Map (empty, fromList)
import           Data.Map ((!))
import           Data.Text (singleton)
import           Data.List (find, findIndex)
import           Control.Lens ((&), (%~), element)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           KeyState (isPressed)

import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
import           Game.Wrapper.Apecs (Apecs(..))

import           Game.System.Input (maintainInputs)
import           Game.Util.Render (loadTexture, toTexture, renderText)
import           Game.Types
  ( Position(..)
  , Font(..)
  , VideoConfig(..)
  , OptionList(..)
  , Option(..)
  , Unit(..)
  , PlayerInput(..) )

-- class Monad m => Scene m where
--   step       :: m ()
--   render     :: m ()
--   transition :: m ()
--   cleanUp    :: m ()

characters =
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ [' ', ':', ',', '-', '.', '>', '<']

type OptionMenu = (OptionList, Position)

oIdAction :: (MonadIO m) => String -> m ()
oIdAction oId = do
  liftIO $ putStrLn $ "Running " ++ oId ++ "!"
  case oId of
    "ToScene_Play"       -> return ()
    "ToScene_SelectFile" -> return ()
    "ToScene_Options"    -> return ()
    "ToScene_Quit"       -> return ()
    _                    -> return ()

titleOptions :: [Option]
titleOptions = [  Option { oId = "ToScene_Play"
                         , text = "New Game"
                         , selected = True }
                , Option { oId = "ToScene_SelectFile"
                         , text = "Load Game"
                         , selected = False }
                , Option { oId = "ToScene_Options"
                         , text = "Options"
                         , selected = False }
                , Option { oId = "ToScene_Quit"
                         , text = "Quit Game"
                         , selected = False }
                ]

titleTransition :: ( Apecs m
                   , HasVideoConfig m
                   , MonadIO m
                   ) => m ()
titleTransition = do
  liftIO $ putStrLn "Title Transition"
  renderer  <- vcRenderer <$> getVideoConfig
  -- load in assets, convert to textures
  smallFont <- liftIO $ TTF.load "assets/fonts/04B_19__.TTF" 24
  fontMap   <- liftIO $ mapM (\c -> do
        texture <- toTexture renderer =<< TTF.blended
          smallFont
          (V4 255 255 255 255)
          (singleton c)
        return (c, texture)
      ) $ characters
  -- after we convert our font to textures we dont need the resource anymore
  TTF.free smallFont
  -- entities
  newEntity ( -- small font
      Position $ V2 0 0
    , Font fontMap )
  newEntity ( -- options menu
      Position $ V2 5 5
    , OptionList titleOptions )
  return ()

titleCleanUp :: (Apecs m) => m ()
titleCleanUp = do
  -- delete the options menu
  cmapM_ $ \(_ :: OptionMenu, ety) -> destroy ety (proxy :: OptionMenu)
  return ()

toggleOptionSelected :: Option -> Option
toggleOptionSelected op = op { selected = not $ selected op }

titleStep :: (Apecs m, MonadIO m) => m ()
titleStep = do
  -- ensure inputs are continually updated
  cmap maintainInputs
  -- when up or down key pressed, shift the selected option up or down
  -- when enter key pressed, use the selected oId to an event fn
  cmapM_ $ \(PlayerInput m _) -> do
    let upPress      = isPressed $ m ! SDL.KeycodeW
        downPress    = isPressed $ m ! SDL.KeycodeS
        enterPress   = isPressed $ m ! SDL.KeycodeReturn
    if (not enterPress && not downPress && not upPress)
    then return ()
    else if enterPress
         -- find the selected oId, run its oIdAction
         then cmapM_ $ \ol@(OptionList options) -> do
                case oId <$> find selected options of
                  Just selectedId -> oIdAction selectedId
                  Nothing         -> return ()
         -- increment or decrement selected option
         else if (not upPress && not downPress)
              then return ()
              else do
                cmap $ \(OptionList options) ->
                  let op      = if upPress then (-) else (+)
                      idx     = findIndex selected options
                      -- next index in cyclical list
                      nextIdx = flip mod (length options) <$> (flip op 1 <$> idx)
                  in case (idx, nextIdx) of
                      (Nothing, _) -> OptionList options
                      (_, Nothing) -> OptionList options
                      (Just idx1, Just idx2) ->
                          OptionList
                        $ options
                        & element idx1 %~ toggleOptionSelected
                        & element idx2 %~ toggleOptionSelected
    return ()

titleRender :: (Apecs m, HasVideoConfig m, MonadIO m) => m ()
titleRender = do
  renderer  <- vcRenderer <$> getVideoConfig
  cmapM_ $ \(Font font) -> do
    -- render title
    liftIO $ renderText renderer font (V2 2 3) "Let Sleeping Gods Lie"
    --render options menu
    cmapM_ $ \(OptionList options, Position (V2 px py)) -> do
      -- render each option descending vertically
      mapM_ (\(option, yMod) -> do
          let newY = py + Unit yMod
          liftIO $ renderText renderer font (V2 px newY) (text option)
          -- draw arrow next to selected option
          when (selected option) $ do
            liftIO $ renderText renderer font (V2 (px - Unit 1) newY) ">"
        ) (zip options ( [x | x <- [0.0, 1.0..(fromIntegral $ length options)] ] ))
      return ()
  return ()
