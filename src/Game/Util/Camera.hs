module Game.Util.Camera
  ( CameraEntity
  , cameraViewMatrix
  , cameraProjectionMatrix
  , runCameraAction ) where

import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import           Control.Lens ((&), (%~), _1)
import           Data.Int (Int32)

import Game.Types
  ( Camera(..)
  , Degrees(..)
  , Rotation(..)
  , CameraAction(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , CameraAxes(..)
  , Position3D(..)
  , ProjectionMatrix(..)
  , ViewMatrix(..) )

type CameraEntity = (Camera, Position3D, Orientation)

-- transformations to camera perspective
cameraViewMatrix :: CameraEntity -> ViewMatrix
cameraViewMatrix (camera :: Camera, Position3D cPos, Orientation o) =
  let q = L.conjugate $ o
  in ViewMatrix $ L.mkTransformation q (L.rotate q $ negate cPos)

cameraProjectionMatrix :: L.V2 Int32 -> CameraEntity -> ProjectionMatrix
cameraProjectionMatrix (L.V2 width' height') (camera, _, _) =
  let height = fromIntegral height'
      width  = fromIntegral width'
      FieldOfView fov   = _fieldOfView camera
  in ProjectionMatrix $ U.projectionMatrix
                          fov
                          (width / height)
                          (_near . _clippingPlanes $ camera)
                          (_far  . _clippingPlanes $ camera)


-- camera action commands
runCameraAction :: CameraAction -> (CameraEntity -> CameraEntity)
runCameraAction (Camera'Dolly    v)    = dollyCamera  v
runCameraAction (Camera'Rotation r d)  = rotateCamera r d
runCameraAction (Camera'Compose a1 a2) = runCameraAction a2 .
                                         runCameraAction a1

dollyCamera :: L.V3 Float -> (CameraEntity -> CameraEntity)
dollyCamera tr (c, Position3D pos, o@(Orientation q)) =
  (c, Position3D $ pos + L.rotate q tr, o)

rotateCamera :: Rotation -> Degrees -> (CameraEntity -> CameraEntity)
rotateCamera r (Degrees d) (c, p, Orientation q) =
  let rot = axis . _cameraAxes $ c
      rad = U.deg2rad d
  in (c, p, Orientation $ q * L.axisAngle rot rad)
  where axis = case r of Pan  -> _yAxis
                         Tilt -> _xAxis
                         Roll -> _zAxis
