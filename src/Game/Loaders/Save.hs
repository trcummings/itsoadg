{-# LANGUAGE FlexibleInstances #-}

module Game.Loaders.Save (saveDataFile, loadDataFile) where

import qualified Data.ByteString.Lazy as B
import           Linear          (V3(..), Quaternion(..))
import           System.FilePath ((</>))
import           System.IO       (hPutStrLn, stderr)
import           System.Exit     (exitFailure)
import           Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , withObject
  , object
  , encode
  , decode
  , (.=)
  , (.:) )

import           Game.Util.Constants (dataPath)
import           Game.Types
  (
    Position3D(..)
  , Orientation(..)
  , Camera(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , CameraAxes(..) )

saveDataFile :: ToJSON a => a -> FilePath -> IO ()
saveDataFile obj path = do
  let fullPath = dataPath </> path
      bStrJson = encode obj
  putStrLn $ "Saving file " ++ "\"" ++ path ++ "\"..."
  B.writeFile fullPath bStrJson
  putStrLn $ "Saved file at " ++ "\"" ++ fullPath ++ "\"!"
  return ()

loadDataFile :: FromJSON a => FilePath -> IO a
loadDataFile path = do
  let fullPath = dataPath </> path
  putStrLn $ "Loading file from " ++ "\"" ++ fullPath ++ "\"..."
  obj <- decode <$> B.readFile fullPath
  case obj of
    Just obj' -> do
      putStrLn $ "Loaded file " ++ "\"" ++ path ++ "\"!"
      return obj'
    _         -> do
      hPutStrLn stderr $ "Error: " ++ fullPath ++ " failed to decode properly"
      exitFailure

-- V3
instance ToJSON (V3 Float) where
  toJSON (V3 x y z) = object ["V3_x" .= x, "V3_y" .= y, "V3_z" .= z]

instance FromJSON (V3 Float) where
  parseJSON = withObject "V3" $ \v ->
    V3 <$> (v .: "V3_x")
       <*> (v .: "V3_y")
       <*> (v .: "V3_z")


-- Position3D
instance ToJSON Position3D where
  toJSON (Position3D pos) = object ["_Position3D" .= toJSON pos]

instance FromJSON Position3D where
  parseJSON = withObject "Position3D" $ \v ->
    Position3D <$> (v .: "_Position3D" >>= parseJSON)


-- Orientation
instance ToJSON Orientation where
  toJSON (Orientation (Quaternion w v3)) =
    object ["Orientation_w" .= w, "Orientation_v3" .= toJSON v3]

instance FromJSON Orientation where
  parseJSON = withObject "Orientation" $ \v ->
    Orientation
      <$> ( Quaternion
        <$> (v .: "Orientation_w")
        <*> (v .: "Orientation_v3" >>= parseJSON) )


-- ClippingPlanes
instance ToJSON ClippingPlanes where
  toJSON (ClippingPlanes { _near = near, _far = far }) =
    object [ "ClippingPlanes_near" .= near
           , "ClippingPlanes_far"  .= far  ]

instance FromJSON ClippingPlanes where
  parseJSON = withObject "ClippingPlanes" $ \v ->
    ClippingPlanes <$> (v .: "ClippingPlanes_near")
                   <*> (v .: "ClippingPlanes_far")


-- FieldOfView
instance ToJSON FieldOfView where
  toJSON (FieldOfView fov) = object ["FieldOfView_fov" .= fov]

instance FromJSON FieldOfView where
  parseJSON = withObject "FieldOfView_fov" $ \v ->
    FieldOfView <$> (v .: "FieldOfView_fov")


-- CameraAxes
instance ToJSON CameraAxes where
  toJSON (CameraAxes { _xAxis = v3x
                     , _yAxis = v3y
                     , _zAxis = v3z }) =
    object [ "CameraAxes_xAxis" .= toJSON v3x
           , "CameraAxes_yAxis" .= toJSON v3y
           , "CameraAxes_zAxis" .= toJSON v3z ]

instance FromJSON CameraAxes where
  parseJSON = withObject "CameraAxes" $ \v ->
    CameraAxes <$> (v .: "CameraAxes_xAxis" >>= parseJSON)
               <*> (v .: "CameraAxes_yAxis" >>= parseJSON)
               <*> (v .: "CameraAxes_zAxis" >>= parseJSON)


-- Camera
instance ToJSON Camera where
  toJSON (Camera { _clippingPlanes = cp
                 , _fieldOfView    = fov
                 , _cameraAxes     = cAx }) =
    object [ "Camera_clippingPlanes" .= toJSON cp
           , "Camera_fieldOfView"    .= toJSON fov
           , "Camera_cameraAxes"     .= toJSON cAx ]

instance FromJSON Camera where
  parseJSON = withObject "Camera" $ \v ->
    Camera <$> (v .: "Camera_clippingPlanes" >>= parseJSON)
           <*> (v .: "Camera_fieldOfView"    >>= parseJSON)
           <*> (v .: "Camera_cameraAxes"     >>= parseJSON)
