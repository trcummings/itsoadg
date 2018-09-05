module Game.System.OptionMenu where

import qualified SDL
import           Apecs

import           Data.Map ((!), keys)
import           Data.List (find, findIndex)
import           Control.Lens ((&), (%~), element)
import           Control.Monad.IO.Class (liftIO)
import           KeyState (isPressed)

import           Game.World.TH (ECS)
import           Game.Effect.Input (getInputs)
import           Game.Types
  ( OptionList(..)
  , Option(..)
  , ActiveOptionList(..)
  , HasOptionMenuEvent(..)
  , OptionMenuCommand(..)
  , Inputs(..)
  , SceneControl(..)
  , PlayerInput(..)
  , Scene(..) )

oIdAction :: String -> ECS ()
oIdAction oId = do
  liftIO $ putStrLn $ "Running Option Menu Action with oId: " ++ oId ++ "!"
  sc <- get global :: ECS SceneControl
  set global $ case oId of
      "ToScene_Play"       -> sc { _nextScene = Scene'Play }
      -- "ToScene_SelectFile" -> return ()
      -- "ToScene_Options"    -> return ()
      "ToScene_Quit"       -> sc { _nextScene = Scene'Quit }
      _                    -> sc

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

initOptionsMenu :: ECS ()
initOptionsMenu = do
  -- options menu
  newEntity (
      -- Position $ V2 5 5
      OptionList titleOptions
    , ActiveOptionList )
  return ()

cleanUpOptionsMenu :: ECS ()
cleanUpOptionsMenu = do
  -- delete the options menu
  -- FIXME: this is laborious. is there a better way?
  cmap $ \(_ :: ActiveOptionList  ) -> Not :: Not ActiveOptionList
  cmap $ \(_ :: HasOptionMenuEvent) -> Not :: Not HasOptionMenuEvent
  cmap $ \(_ :: OptionList        ) -> Not :: Not OptionList

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

stepOptionMenu :: ECS ()
stepOptionMenu = do
  -- when up or down key pressed, shift the selected option up or down
  -- when enter key pressed, use the selected oId to an event fn
  ipts <- getInputs
  let m          = inputs . _keyboardInput $ ipts
      upPress    = isPressed $ m ! SDL.KeycodeW
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
  cmapM $ \( ActiveOptionList
           , HasOptionMenuEvent command
           , OptionList options ) ->
    case command of
      -- find the selected oId, run its oIdAction, remove its event
      SelectOption -> do
        case oId <$> find selected options of
          Just selectedId -> oIdAction selectedId
          Nothing         -> return ()
        return $ Left  $ ( Not :: Not HasOptionMenuEvent )
      -- otherwise, update the option, remove its event
      _            -> do
        return $ Right $ ( updateOption command $ OptionList options
                         , Not :: Not HasOptionMenuEvent )


renderOptionMenu :: ECS ()
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
