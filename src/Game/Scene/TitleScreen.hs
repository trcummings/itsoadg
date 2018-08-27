{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Game.Scene.TitleScreen where

import qualified SDL
import           SDL.Font  as TTF (free, load, blended)
import           Linear (V4(..), V2(..))
import qualified Data.Map as Map (empty, fromList)
import           Data.Text (singleton)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Apecs (newEntity)

import           Game.System.Render (loadTexture, toTexture)
import           Game.World (System')
import           Game.Types (Position(..), Font(..))

characters =
     ['a'..'z']
  ++ ['A'..'Z']
  ++ ['0'..'9']
  ++ [' ', ':', ',', '-', '.']

initSystems :: SDL.Renderer -> System' ()
initSystems renderer = void $ do
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
