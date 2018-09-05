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
import           Game.Util.Constants  (frameDeltaSeconds)

import           Game.World.TH (ECS)
import           Game.Util.Billboard (renderBillboard)
import           Game.Util.Texture (getAndCreateTexture)
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
  , Position3D(..)
  , Model(..)
  , Resource(..)
  , Unit(..)
  , Scene(..) )

shaderPath :: FilePath
shaderPath = "assets" </> "glsl"

texturePath :: FilePath
texturePath = "assets" </> "sprites"

cubeVertices = L.V3 <$> [1, -1] <*> [1, -1] <*> [1, -1]

initialize :: ECS ()
initialize = do
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
      Model { _resource = resrce
            , _vertices = verts
            , _colors   = cols
            , _elements = elems }
    , Position3D $ L.V3 0 0 (-4) )

  -- player character
  let p = texturePath </> "player_char.tga"
  playerTexture <- liftIO $ getAndCreateTexture p
  newEntity (
      Player playerTexture
    , Position3D $ L.V3 0 0 (-2) )

  -- camera
  newEntity (
      Camera { _clippingPlanes = ClippingPlanes { _near = 0.1, _far = 10 }
             , _fieldOfView    = FieldOfView (pi / 4)
             , _orientation    = Orientation $ L.Quaternion 1 (L.V3 0 0 0)
             , _cameraAxes     = CameraAxes { _xAxis = L.V3 1 0 0
                                            , _yAxis = L.V3 0 1 0
                                            , _zAxis = L.V3 0 0 (-1) } }
    , Position3D $ L.V3 0 0 0 )
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
  cmap $ \(_ :: Model, _ :: Position3D) -> Not :: Not (Model, Position3D)
  -- destroy the camera
  cmap $ \(_ :: CameraEntity  ) -> Not :: Not CameraEntity
  cmap $ \(_ :: HasCameraEvent) -> Not :: Not HasCameraEvent
  -- destroy the player

step :: ECS ()
step = do
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
  t <- getGlobalTime
  cmapM_ $ \(camera :: CameraEntity) -> do
    cmapM_ $ \(model :: Model, Position3D mPos) -> do
      let dgt       = t / 1000
          cubeModel = L.mkTransformationMat L.identity mPos
          -- the cube
          -- cubeAnim  = L.mkTransformation (L.axisAngle
          --                                   (L.V3 0 1 0)
          --                                   (realToFrac dgt * pi / 4))
          --                                L.zero
          cubeTrans = (cameraProjectionMatrix dims camera)
                  !*! (cameraViewMatrix camera)
                  !*! cubeModel
                      -- !*! cubeAnim


      let r = _resource model
          v = _vertices model
          c = _colors   model
          e = _elements model
          sProgram    = _shaderProgram r
          attribKeys  = keys $ U.attribs  sProgram
          uniformKeys = keys $ U.uniforms sProgram
      -- set current program to shaderProgram
      liftIO $ GL.currentProgram $= (Just $ U.program sProgram)
      -- enable all attributes
      liftIO $ mapM_ (\k -> liftIO $ U.enableAttrib sProgram k) attribKeys
      -- bind all buffers & set all attributes
      liftIO $ GL.bindBuffer GL.ArrayBuffer $= Just (_vertBuffer r)
      liftIO $ U.setAttrib
          sProgram
          "coord3d"
          GL.ToFloat $
          GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
      liftIO $ GL.bindBuffer GL.ArrayBuffer $= Just (_colorBuffer r)
      liftIO $ U.setAttrib
          sProgram
          "v_color"
          GL.ToFloat $
          GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
      -- transform all uniforms
      liftIO $ mapM_ (\k -> do
            U.asUniform $ cubeTrans
          $ U.getUniform sProgram k
        ) uniformKeys
      -- -- draw player
      -- cmapM_ $ \(Player pt, pPos :: Position3D) -> do
      --   -- liftIO $ GL.clear [GL.DepthBuffer]
      --   liftIO $ renderBillboard pPos pt
      -- bind element buffer
      liftIO $ GL.bindBuffer GL.ElementArrayBuffer $= Just (_elementBuffer r)
      -- draw indexed triangles
      liftIO $ U.drawIndexedTris (fromIntegral $ length e)

      -- disable all attributes
      liftIO $ mapM_ (\k -> do
          GL.vertexAttribArray (U.getAttrib sProgram k) $= GL.Disabled
        ) attribKeys

    -- cmapM_ $ \(Player pt) -> do
    --   liftIO $ GL.clear [GL.DepthBuffer]
    --   liftIO $ renderBillboard pt

  -- -- renderer  <- vcRenderer <$> getVideoConfig
  -- -- cmapM_ $ \(Font font) -> do
  -- --   -- render title
  -- --   liftIO $ renderText renderer font (V2 2 3) "Let Sleeping Gods Lie"
  -- --   --render options menu
  -- --   cmapM_ $ \(OptionList options, Position (V2 px py)) -> do
  -- --     -- render each option descending vertically
  -- --     mapM_ (\(option, yMod) -> do
  -- --         let newY = py + Unit yMod
  -- --         liftIO $ renderText renderer font (V2 px newY) (text option)
  -- --         -- draw arrow next to selected option
  -- --         when (selected option) $ do
  -- --           liftIO $ renderText renderer font (V2 (px - Unit 1) newY) ">"
  -- --       ) (zip options ( [x | x <- [0.0, 1.0..(fromIntegral $ length options)] ] ))
  -- --     return ()
  return ()
