module Game.Scene.Play where

import qualified SDL
import           SDL (($=))
import           Apecs
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
import           Control.Monad (when, mapM_)
import           Control.Monad.IO.Class (liftIO)
import           KeyState (isPressed, isTouched)
import           Control.Applicative
import           System.FilePath ((</>))

import           Game.Effect.Clock    (getGlobalTime)
import           Game.Effect.Input    (getInputs)
import           Game.Effect.Renderer (getWindowDims)

import           Game.Loaders.Obj.Loader (loadObjFile)

import           Game.World.TH        (ECS)
import           Game.Util.Billboard  (renderBillboard)
import           Game.Util.Texture    (getAndCreateTexture)
import           Game.Util.Constants
  ( frameDeltaSeconds
  , shaderPath
  , texturePath
  , objPath )
import           Game.Util.Camera
  ( cameraViewMatrix
  , cameraProjectionMatrix
  , runCameraAction
  , CameraEntity )
import           Game.Types
  ( VideoConfig(..)
  , PlayerInput(..)
  , Inputs(..)
  , SceneControl(..)

  , HasCameraEvent(..)
  , CameraAction(..)
  , Degrees(..)
  , Rotation(..)
  , Camera(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , CameraAxes(..)

  , Player(..)
  , RotatingCube(..)
  , Position3D(..)
  , Model(..)
  , Resource(..)
  , Unit(..)
  , Scene(..) )
import Game.System.ColorCube
  ( ColorCube
  , initColorCube
  , stepColorCube
  , drawColorCube )

initialize :: ECS ()
initialize = do
  initColorCube

  obj <- liftIO $ loadObjFile $ objPath </> "cube.obj"
  liftIO $ putStrLn $ show obj
  -- -- player character
  -- let p = texturePath </> "player_char.tga"
  -- playerTexture <- liftIO $ getAndCreateTexture p
  -- newEntity (
  --     Player playerTexture
  --   , Position3D $ L.V3 0 0 (-2) )

  -- camera
  newEntity (
      Camera { _clippingPlanes = ClippingPlanes { _near = 0.1, _far = 10 }
             , _fieldOfView    = FieldOfView (pi / 4)
             , _cameraAxes     = CameraAxes { _xAxis = L.V3 1 0 0
                                            , _yAxis = L.V3 0 1 0
                                            , _zAxis = L.V3 0 0 (-1) } }
    , Position3D  $ L.V3 0 0 0
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )
  -- move up, tilt down to look at cube
  -- cmap $ \(c :: CameraEntity) ->
  --     runCameraAction (
  --       Camera'Compose
  --         (Camera'Rotation Tilt (Degrees (-30)))
  --         (Camera'Dolly (L.V3 0 2 0))) c
  return ()

cleanUp :: ECS ()
cleanUp = do
  -- destroy the cube
  -- FIXME: need to free the Model resources
  -- cmap $ \(_ :: Model, _ :: Position3D) -> Not :: Not (Model, Position3D)
  -- destroy the camera
  cmap $ \(_ :: CameraEntity  ) -> Not :: Not CameraEntity
  cmap $ \(_ :: HasCameraEvent) -> Not :: Not HasCameraEvent
  -- destroy the player

step :: ECS ()
step = do
  -- rotate da cubes!!!
  cmap stepColorCube

  -- if escape pressed, transition to quit
  inputs <- getInputs
  let m = _inputs . _keyboardInput $ inputs
  when (isPressed $ m ! SDL.KeycodeEscape) $ do
    sc <- get global :: ECS SceneControl
    set global $ sc { _nextScene = Scene'Quit }

  -- send camera events based on WASD presses
  cmap $ \( c :: Camera
          , _ :: Not HasCameraEvent ) ->
    let leftPress    = isTouched $ m ! SDL.KeycodeA
        rightPress   = isTouched $ m ! SDL.KeycodeD
        forwardPress = isTouched $ m ! SDL.KeycodeW
        backPress    = isTouched $ m ! SDL.KeycodeS
        t            = realToFrac frameDeltaSeconds :: Float
        toDolly v    = Camera'Dolly $ v L.^* t
        toRotat r    = Camera'Rotation Pan (Degrees $ r * t)
        rt = if leftPress && rightPress
             then Nothing
             else if leftPress
                  then Just $ toRotat 90
                  else if rightPress
                       then Just $ toRotat (-90)
                       else Nothing
        vx = if forwardPress && backPress
             then Nothing
             else if backPress
                  then Just $ toDolly $ L.V3 0 0 1
                  else if forwardPress
                       then Just $ toDolly $ L.V3 0 0 (-1)
                       else Nothing
    in case (vx, rt) of
        (Nothing , Nothing ) -> Left c
        (Just vx', Just rt') -> Right (c, HasCameraEvent (Camera'Compose vx' rt'))
        (Just vx', _       ) -> Right (c, HasCameraEvent vx')
        (_       , Just rt') -> Right (c, HasCameraEvent rt')

  -- resolve camera movement based on camera events
  cmap $ \(HasCameraEvent e, (c :: CameraEntity)) ->
    ( runCameraAction e c
    , Not :: Not HasCameraEvent )

render :: ECS ()
render = do
  -- render cube
  vc <- get global :: ECS VideoConfig
  dims <- liftIO $ getWindowDims vc
  -- t <- getGlobalTime
  cmapM_ $ \(camera :: CameraEntity) -> do
    let camProjMatrix = cameraProjectionMatrix dims camera
        camViewMatrix = cameraViewMatrix camera
    cmapM_ $ \(r :: ColorCube) -> do
      liftIO $ drawColorCube (camProjMatrix, camViewMatrix) r
