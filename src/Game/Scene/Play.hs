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

import           Game.World.TH        (ECS)
import           Game.Util.Billboard  (renderBillboard)
import           Game.Util.Texture    (getAndCreateTexture)
import           Game.Util.Constants
  ( frameDeltaSeconds
  , shaderPath
  , texturePath )
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

initialize :: ECS ()
initialize = do
  let vertexShader   = shaderPath </> "cube.v.glsl"
      fragmentShader = shaderPath </> "cube.f.glsl"
      vertices       = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]
      colors         = vertices
      elements       = [ L.V3 2 1 0 -- right
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
  shaderProgram <- liftIO $ U.simpleShaderProgram vertexShader fragmentShader
  vertexBuffer  <- liftIO $ U.fromSource GL.ArrayBuffer vertices
  colorBuffer   <- liftIO $ U.fromSource GL.ArrayBuffer colors
  elementBuffer <- liftIO $ U.fromSource GL.ElementArrayBuffer elements
  let resource = Resource { _shaderProgram = shaderProgram
                          , _vertexBuffer  = vertexBuffer
                          , _colorBuffer   = colorBuffer
                          , _elementBuffer = elementBuffer }
      model = Model { _resource = resource
                    , _vertices = vertices
                    , _colors   = colors
                    , _elements = elements }

  -- cube 1
  newEntity (
      RotatingCube { _axis = L.V3 0 0 (-1)
                   , _deg  = Degrees 4 }
    , model
    , Position3D  $ L.V3 0 0 (-4)
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )

  -- cube 2
  newEntity (
      RotatingCube { _axis = L.V3 1 0 0
                   , _deg  = Degrees (-2) }
    , model
    , Position3D  $ L.V3 2 1 (-3)
    , Orientation $ L.Quaternion 1 (L.V3 0 0 0) )
  --
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
  cmap $ \(rc :: RotatingCube, Orientation o) ->
    let Degrees deg = _deg rc
    in  Orientation $ o * L.axisAngle (_axis rc) (U.deg2rad deg)

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

newtype ProjectionMatrix = ProjectionMatrix (L.M44 Float)
newtype ViewMatrix       = ViewMatrix       (L.M44 Float)
type Cube3D = (RotatingCube, Model, Position3D, Orientation)

renderProgram :: (ProjectionMatrix, ViewMatrix)
              -> Cube3D
              -> IO ()
renderProgram (ProjectionMatrix projMatrix, ViewMatrix viewMatrix)
              (_, model, Position3D mPos, Orientation o) = do
  let modelMatrix   = L.mkTransformation o mPos
      trans         = projMatrix
                  !*! viewMatrix
                  !*! modelMatrix
      resource      = _resource model
      vertices      = _vertices model
      colors        = _colors   model
      elements      = _elements model
      shaderProgram = _shaderProgram resource
      attribKeys    = keys $ U.attribs  shaderProgram
      uniformKeys   = keys $ U.uniforms shaderProgram
  -- set current program to shaderProgram
  GL.currentProgram $= (Just $ U.program shaderProgram)
  -- enable all attributes
  mapM_ (U.enableAttrib shaderProgram) attribKeys
  -- bind all buffers & set all attributes
  GL.bindBuffer GL.ArrayBuffer $= Just (_vertexBuffer resource)

  U.setAttrib
      shaderProgram
      "coord3d"
      GL.ToFloat $
      GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  GL.bindBuffer GL.ArrayBuffer $= Just (_colorBuffer resource)

  U.setAttrib
      shaderProgram
      "v_color"
      GL.ToFloat $
      GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  -- transform all uniforms
  mapM_ (\keys -> do
        U.asUniform $ trans
      $ U.getUniform shaderProgram keys
    ) uniformKeys

  -- bind element buffer
  GL.bindBuffer
      GL.ElementArrayBuffer $= Just (_elementBuffer resource)

  -- draw indexed triangles
  U.drawIndexedTris (fromIntegral $ length elements)

  -- disable all attributes
  mapM_ (\key -> do
      GL.vertexAttribArray (U.getAttrib shaderProgram key) $= GL.Disabled
    ) attribKeys


render :: ECS ()
render = do
  -- render cube
  vc <- get global :: ECS VideoConfig
  dims <- liftIO $ getWindowDims vc
  -- t <- getGlobalTime
  cmapM_ $ \(camera :: CameraEntity) -> do
    let camProjMatrix = ProjectionMatrix $ cameraProjectionMatrix dims camera
        camViewMatrix = ViewMatrix $ cameraViewMatrix camera
    cmapM_ $ \(r :: Cube3D) -> do
      liftIO $ renderProgram (camProjMatrix, camViewMatrix) r
