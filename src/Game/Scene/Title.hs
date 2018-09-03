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
import           SDL (($=))
-- import           Apecs
import qualified Linear as L
import           Linear ((!*!))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.Map as Map (empty, fromList)
import           Data.Map ((!), keys)
import           Data.Text (singleton)
import           Data.List (find, findIndex)
import           Data.Coerce (coerce)
import           Control.Lens ((&), (%~), element)
import           Control.Monad (void, when, mapM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           KeyState (isPressed)
import           Control.Applicative
import           System.FilePath ((</>))

import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
import           Game.Effect.SceneManager (SceneManager, setNextScene, getNextScene)
import           Game.Effect.Clock (Clock, getGlobalTime)
import           Game.Effect.Input (Input, updateInputs, getInputs)
-- import           Game.Wrapper.Apecs (Apecs(..))
-- import           Game.System.OptionMenu ( initOptionsMenu
--                                         , cleanUpOptionsMenu
--                                         , renderOptionMenu
--                                         , stepOptionMenu )
import           Game.Types
  ( VideoConfig(..)

  , Camera(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , CameraAxes(..)

  , Position3D(..)
  , Model(..)
  , Resource(..)
  , Unit(..)
  , Scene(..) )
import Game.World.TH (initWorld, World)

-- initTitle :: (Apecs m, MonadIO m) => m ()
initTitle :: MonadIO m => m ()
initTitle = do
  -- world <- liftIO $ initWorld
  -- liftIO $ runWith world $ void $ newEntity (Position3D $ L.V3 0 0 0)
  -- liftIO $ putStrLn "Initialize Title"
  -- -- options menu
  -- initOptionsMenu
  return ()

-- cleanUpTitle :: (Apecs m, MonadIO m) => m ()
cleanUpTitle :: MonadIO m => m ()
cleanUpTitle = do
  liftIO $ putStrLn "Clean Up Title"
  -- -- delete the options menu
  -- cleanUpOptionsMenu
  -- return ()

-- stepTitle :: ( Apecs m
--              , Input m
--              , SceneManager m
--              , MonadIO m
--              ) => m ()
stepTitle :: (Input m, SceneManager m) => m ()
stepTitle = do
  -- -- ensure inputs are continually updated
  -- updateInputs
  -- -- step option menu
  -- stepOptionMenu
  return ()


-- renderTitle :: (Apecs m, Clock m, HasVideoConfig m, MonadIO m) => m ()
renderTitle :: (Clock m, HasVideoConfig m, MonadIO m) => m ()
renderTitle = do
  -- -- render option menu
  -- renderOptionMenu
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
