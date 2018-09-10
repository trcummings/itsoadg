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
import           System.FilePath        ((</>))

import           Game.Effect.Clock    (getGlobalTime)
import           Game.Effect.Input    (getInputs)
import           Game.Effect.Renderer (getWindowDims)

import           Game.World.TH        (ECS)
import           Game.Loaders.Save    (saveDataFile, loadDataFile)
import           Game.Util.Billboard  (renderBillboard)
import           Game.Util.Constants  (frameDeltaSeconds, assetPath)
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

  , Position3D(..)
  , Scene(..) )
import Game.System.Scratch.VAO (initVAO, cleanUpVAO)
import Game.System.Scratch.ColorCube
  ( ColorCube
  , initColorCube
  , stepColorCube
  , drawColorCube )
import Game.System.Scratch.TextureCube
  ( TexCube
  , initTextureCube
  , drawTextureCube )
import Game.System.Scratch.PlayerBillboard
  ( PlayerB
  , initPlayerBillboard
  , drawPlayerBillboard )
import Game.Loaders.Cfg (readMapCfg, readMapMedia)

initialize :: ECS ()
initialize = do
  -- init VAO
  initVAO
  -- load config data
  liftIO $ readMapCfg   $ assetPath </> "leveleg.cfg"
  liftIO $ readMapMedia $ assetPath </> "leveleg.med"
  -- entities
  initColorCube
  initTextureCube
  initPlayerBillboard

  -- camera
  cam :: CameraEntity <- liftIO $ loadDataFile "test.json"
  -- let cam = (
  --         Camera { _clippingPlanes = ClippingPlanes { _near = 0.1, _far = 10 }
  --                , _fieldOfView    = FieldOfView (pi / 4)
  --                , _cameraAxes     = CameraAxes { _xAxis = L.V3 1 0 0
  --                                               , _yAxis = L.V3 0 1 0
  --                                               , _zAxis = L.V3 0 0 (-1) } }
  --       , Position3D  $ L.V3 0 1 4
  --       , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )
  -- liftIO $ saveDataFile cam "test.json"
  newEntity cam
  -- move up, tilt down to look at cube
  -- cmap $ (runCameraAction $ Camera'Dolly (L.V3 0 1 4))
  -- cmap $ \(c :: CameraEntity) ->
  --     runCameraAction (
  --       Camera'Compose
  --         (Camera'Rotation Tilt (Degrees (-45)))
  --         (Camera'Dolly (L.V3 0 2 4))) c
  return ()

cleanUp :: ECS ()
cleanUp = do
  -- destroy the cube
  -- FIXME: need to free the Model resources
  -- cmap $ \(_ :: Model, _ :: Position3D) -> Not :: Not (Model, Position3D)
  -- destroy the camera
  cmap $ \(_ :: CameraEntity  ) -> Not :: Not CameraEntity
  cmap $ \(_ :: HasCameraEvent) -> Not :: Not HasCameraEvent
  cleanUpVAO

quitOnEsc :: Inputs -> ECS ()
quitOnEsc inputs = do
  let m = _inputs . _keyboardInput $ inputs
  when (isPressed $ m ! SDL.KeycodeEscape) $ do
    sc <- get global :: ECS SceneControl
    set global $ sc { _nextScene = Scene'Quit }

sendCameraMoveEvents :: Inputs
                     -> (Camera, Not HasCameraEvent)
                     -> Either Camera (Camera, HasCameraEvent)
sendCameraMoveEvents inputs (c, _) =
  let m            = _inputs . _keyboardInput $ inputs
      leftPress    = isTouched $ m ! SDL.KeycodeA
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

runCameraActions :: (HasCameraEvent, CameraEntity)
                 -> (CameraEntity, Not HasCameraEvent)
runCameraActions (HasCameraEvent e, c) =
  (runCameraAction e c, Not :: Not HasCameraEvent)

step :: ECS ()
step = do
  -- rotate da cubes!!!
  cmap stepColorCube
  -- get inputs
  inputs <- getInputs
  -- if escape pressed, transition to quit
  quitOnEsc inputs
  -- send camera events based on WASD presses
  cmap $ sendCameraMoveEvents inputs
  -- resolve camera movement based on camera events
  cmap $ runCameraActions

render :: ECS ()
render = do
  -- render cube
  vc <- get global :: ECS VideoConfig
  dims <- liftIO $ getWindowDims vc
  -- t <- getGlobalTime
  cmapM_ $ \(camera :: CameraEntity) -> do
    let camProjMatrix = cameraProjectionMatrix dims camera
        camViewMatrix = cameraViewMatrix camera
        mats          = (camProjMatrix, camViewMatrix)
    cmapM_ $ \(r :: ColorCube) -> liftIO $ drawColorCube       mats r
    cmapM_ $ \(r :: TexCube)   -> liftIO $ drawTextureCube     mats r
    cmapM_ $ \(r :: PlayerB)   -> liftIO $ drawPlayerBillboard mats r
    return ()
