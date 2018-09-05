module Game.Util.Camera where

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
  , Position3D(..) )

type CameraEntity = (Camera, Position3D)

-- transformations to camera perspective
cameraViewMatrix :: CameraEntity -> L.M44 Float
cameraViewMatrix (camera :: Camera, Position3D cPos) =
  let Orientation ore   = orientation camera
      q                 = L.conjugate $ ore
  in L.mkTransformation q (L.rotate q $ negate cPos)

cameraProjectionMatrix :: L.V2 Int32 -> CameraEntity -> L.M44 Float
cameraProjectionMatrix (L.V2 width' height')
                       (camera :: Camera, Position3D cPos) =
  let height = fromIntegral height'
      width  = fromIntegral width'
      FieldOfView fov   = fieldOfView camera
  in U.projectionMatrix
      fov
      (width / height)
      (near . clippingPlanes $ camera)
      (far  . clippingPlanes $ camera)


-- camera action commands
runCameraAction :: CameraAction -> (CameraEntity -> CameraEntity)
runCameraAction (Camera'Dolly    v)    = dollyCamera  v
runCameraAction (Camera'Rotation r d)  = rotateCamera r d
runCameraAction (Camera'Compose a1 a2) = runCameraAction a2 .
                                         runCameraAction a1

dollyCamera :: L.V3 Float -> (CameraEntity -> CameraEntity)
dollyCamera tr (c, Position3D pos) = (c, Position3D $ pos + tr')
  where Orientation q = orientation c
        tr'           = L.rotate q tr

rotateCamera :: Rotation -> Degrees -> (CameraEntity -> CameraEntity)
rotateCamera r (Degrees d) c =
  c & _1 %~ (\cam ->
    let Orientation q = orientation cam
        rot           = (axis . cameraAxes $ cam)
        rad           = U.deg2rad d
    in cam { orientation = Orientation $ q * L.axisAngle rot rad })
  where axis = case r of Pan  -> yAxis
                         Tilt -> xAxis
                         Roll -> zAxis
