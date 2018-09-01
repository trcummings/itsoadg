module Game.Util.Camera where

import           Control.Lens ((&), (%~), _1, _2)
-- import           Linear ((!*!))
import qualified Linear as L
-- import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL

import           Game.Types
  ( Camera(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , CameraAxes(..)
  , Position3D(..) )

newtype Degrees = Degrees Float

data Rotation =
    Pan
  | Tilt
  | Roll

data CameraAction =
    Camera'Dolly (L.V3 Float)
  | Camera'Rotation Rotation Degrees

type CameraEntity = (Camera, Position3D)

runCameraAction :: CameraAction -> (CameraEntity -> CameraEntity)
runCameraAction (Camera'Dolly    v)   = dollyCamera  v
runCameraAction (Camera'Rotation r d) = rotateCamera r d

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
