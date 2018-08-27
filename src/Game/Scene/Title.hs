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
import           Linear (V4(..), V2(..))
import qualified Data.Map as Map (empty, fromList)
import           Data.Text (singleton)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
import           Game.Wrapper.Apecs (Apecs(..))

import           Game.System.Render (loadTexture, toTexture)
import           Game.Types (Position(..), Font(..), VideoConfig(..))

characters =
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ [' ', ':', ',', '-', '.']

titleTransition :: ( Apecs m
                   , HasVideoConfig m
                   , MonadIO m
                   ) => m ()
titleTransition = do
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
  return ()

titleStep :: (Apecs m, MonadIO m) => m ()
titleStep = do
  return ()
