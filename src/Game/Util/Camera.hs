module Game.Util.Camera where

import Control.Lens ((&), (%~), _1)
import Linear (V3, axisAngle, rotate)
import Graphics.GLUtil.Camera3D (deg2rad)

import Game.Types
  ( Camera(..)
  , Degrees(..)
  , Rotation(..)
  , CameraAction(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , CameraAxes(..)
  , Position3D(..) )

type CameraEntity = (Camera, Position3D)

runCameraAction :: CameraAction -> (CameraEntity -> CameraEntity)
runCameraAction (Camera'Dolly    v)   = dollyCamera  v
runCameraAction (Camera'Rotation r d) = rotateCamera r d

dollyCamera :: V3 Float -> (CameraEntity -> CameraEntity)
dollyCamera tr (c, Position3D pos) = (c, Position3D $ pos + tr')
  where Orientation q = orientation c
        tr'           = rotate q tr

rotateCamera :: Rotation -> Degrees -> (CameraEntity -> CameraEntity)
rotateCamera r (Degrees d) c =
  c & _1 %~ (\cam ->
    let Orientation q = orientation cam
        rot           = (axis . cameraAxes $ cam)
        rad           = deg2rad d
    in cam { orientation = Orientation $ q * axisAngle rot rad })
  where axis = case r of Pan  -> yAxis
                         Tilt -> xAxis
                         Roll -> zAxis
