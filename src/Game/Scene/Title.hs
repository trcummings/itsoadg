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
import           Apecs (proxy, global)
import qualified Linear as L
import           Linear ((!*!))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.Map as Map (empty, fromList)
import           Data.Map ((!), keys)
import           Data.Text (singleton)
import           Data.List (find, findIndex)
import           Control.Lens ((&), (%~), element)
import           Control.Monad (when, mapM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           KeyState (isPressed)
import           Control.Applicative
import           System.FilePath ((</>))

import           Game.Effect.HasVideoConfig (HasVideoConfig(..))
import           Game.Effect.SceneManager (SceneManager, setNextScene)
import           Game.Wrapper.Apecs (Apecs(..))

import           Game.System.Input (maintainInputs)
import           Game.Types
  ( OptionList(..)
  , Option(..)
  -- , Position(..)
  -- , Font(..)
  , VideoConfig(..)
  -- , Camera(..)
  , GlobalTime(..)
  , Model(..)
  , Resource(..)
  , Unit(..)
  , PlayerInput(..)
  , Scene(..) )

-- class Monad m => Scene m where
--   step       :: m ()
--   render     :: m ()
--   transition :: m ()
--   cleanUp    :: m ()

shaderPath :: FilePath
shaderPath = "assets" </> "glsl"

oIdAction :: (SceneManager m, MonadIO m) => String -> m ()
oIdAction oId = do
  liftIO $ putStrLn $ "Running " ++ oId ++ "!"
  case oId of
    "ToScene_Play"       -> return ()
    "ToScene_SelectFile" -> return ()
    "ToScene_Options"    -> return ()
    "ToScene_Quit"       -> setNextScene Scene'Quit
    _                    -> return ()

transformM :: Int -> Int -> Double -> L.M44 GL.GLfloat
transformM width height t = projection !*! view !*! model !*! anim where
  angle      = realToFrac t * pi/4
  anim       = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
  model      = L.mkTransformationMat L.identity $ L.V3 0 0 (-4)
  view       = U.camMatrix cam
  cam        = U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
  projection = U.projectionMatrix (pi/4) aspect 0.1 10
  aspect     = fromIntegral width / fromIntegral height

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

titleTransition :: (Apecs m, MonadIO m) => m ()
titleTransition = do
  liftIO $ putStrLn "Title Transition"

  -- options menu
  newEntity (
      -- Position $ V2 5 5
      OptionList titleOptions )

   -- cube
  let v = shaderPath </> "cube.v.glsl"
      f = shaderPath </> "cube.f.glsl"
      verts = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]
      cols  = verts
      elems = [ L.V3 2 1 0 -- right
              , L.V3 1 2 3
              , L.V3 0 1 4 -- top
              , L.V3 4 1 5
              , L.V3 4 5 6 -- left
              , L.V3 7 6 5
              , L.V3 2 6 3 -- bottom
              , L.V3 6 3 7
              , L.V3 0 4 2 -- front
              , L.V3 2 4 6
              , L.V3 5 1 7 -- back
              , L.V3 7 1 3
              ]
  resrce <- liftIO $ Resource <$> U.simpleShaderProgram v f
                              <*> U.fromSource GL.ArrayBuffer verts
                              <*> U.fromSource GL.ArrayBuffer cols
                              <*> U.fromSource GL.ElementArrayBuffer elems
  newEntity (
      Model { resource = resrce
            , vertices = verts
            , colors   = cols
            , elements = elems } )
  return ()

titleCleanUp :: (Apecs m) => m ()
titleCleanUp = do
  -- delete the options menu
  cmapM_ $ \(_ :: OptionList, ety) -> destroy ety (proxy :: OptionList)
  -- destroy the cube
  cmapM_ $ \(_ :: Model, ety) -> destroy ety (proxy :: Model)
  return ()

titleStep :: ( Apecs m
             , SceneManager m
             , MonadIO m
             ) => m ()
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
                        & element idx1 %~ (\opt -> opt { selected = not $ selected opt })
                        & element idx2 %~ (\opt -> opt { selected = not $ selected opt })
    return ()

titleRender :: (Apecs m, HasVideoConfig m, MonadIO m) => m ()
titleRender = do
  window <- vcWindow <$> getVideoConfig
  (L.V2 width' height') <- SDL.get $ SDL.windowSize window
  cmapM_ $ \(model :: Model, GlobalTime t) -> do
    let r = resource model
        v = vertices model
        c = colors   model
        e = elements model
        dgt = t / 1000
        sProgram    = shaderProgram r
        attribKeys  = keys $ U.attribs sProgram
        uniformKeys = keys $ U.uniforms sProgram
        height = fromIntegral height'
        width  = fromIntegral width'

    -- set current program to shaderProgram
    liftIO $ GL.currentProgram $= (Just $ U.program sProgram)

    -- enable all attributes
    liftIO $ mapM_ (\k -> liftIO $ U.enableAttrib sProgram k) attribKeys

    -- bind all buffers & set all attributes
    liftIO $ GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
    liftIO $ U.setAttrib sProgram "coord3d"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

    liftIO $ GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
    liftIO $ U.setAttrib sProgram "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

    -- transform all uniforms
    liftIO $ mapM_ (\k -> do
          U.asUniform (transformM width height dgt)
        $ U.getUniform sProgram k
      ) uniformKeys

    -- bind element buffer
    liftIO $ GL.bindBuffer GL.ElementArrayBuffer $= Just (elementBuffer r)

    -- draw indexed triangles
    liftIO $ U.drawIndexedTris (fromIntegral $ length e)

    -- disable all attributes
    liftIO $ mapM_ (\k -> do
        GL.vertexAttribArray (U.getAttrib sProgram k) $= GL.Disabled
      ) attribKeys
    return ()


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
