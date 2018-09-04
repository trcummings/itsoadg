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

import           Game.Effect.Clock (getGlobalTime)
-- import           Game.System.OptionMenu ( initOptionsMenu
--                                         , cleanUpOptionsMenu
--                                         , renderOptionMenu
--                                         , stepOptionMenu )
import           Game.World.TH (ECS)
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

initialize :: ECS ()
initialize = do
  liftIO $ putStrLn "Initialize Title"
  -- options menu
  -- initOptionsMenu

cleanUp :: ECS ()
cleanUp = do
  liftIO $ putStrLn "Clean Up Title"
  -- delete the options menu
  -- cleanUpOptionsMenu


step :: ECS ()
step = do
  return ()
  -- ensure inputs are continually updated
  -- updateInputs
  -- -- step option menu
  -- stepOptionMenu


render :: ECS ()
render = do
  return ()
  -- render option menu
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
