module Game.Util.Camera where

import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Linear as L
import           Data.Int (Int32)

import Game.Util.Move (Moveable)
import Game.Types
  ( Camera(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , Position3D(..)
  , ProjectionMatrix(..)
  , ViewMatrix(..)
  , CamInfo
  )

type CameraEntity = (Camera, Moveable)

_cPos  :: CamInfo -> Position3D
_cPos  (_, _, x) = x

_cPMat :: CamInfo -> ProjectionMatrix
_cPMat (x, _, _) = x

_cVMat :: CamInfo -> ViewMatrix
_cVMat (_, x, _) = x

-- transformations to camera perspective
cameraViewMatrix :: CameraEntity -> ViewMatrix
cameraViewMatrix (camera :: Camera, (Orientation o, Position3D cPos)) =
  let q = L.conjugate $ o
  in ViewMatrix $ L.mkTransformation q (L.rotate q $ negate cPos)

cameraProjectionMatrix :: L.V2 Int32 -> CameraEntity -> ProjectionMatrix
cameraProjectionMatrix (L.V2 width' height') (camera, _) =
  let height = fromIntegral height'
      width  = fromIntegral width'
      FieldOfView fov   = _fieldOfView camera
  in ProjectionMatrix
    $ U.projectionMatrix
        fov
        (width / height)
        (_near . _clippingPlanes $ camera)
        (_far  . _clippingPlanes $ camera)
