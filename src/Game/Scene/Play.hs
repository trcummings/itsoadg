module Game.Scene.Play where

import qualified SDL
import           SDL (($=))
import           Apecs
import qualified Linear as L
import           Linear ((!*!))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.Map as Map (empty, fromList, keys, elems)
import           Data.Map ((!), keys, insert)
import           Data.Text (singleton)
import           Data.List (find, findIndex)
import           Data.Coerce (coerce)
import           Control.Lens ((&), (%~), element)
import           Control.Monad (when, mapM_)
import           Control.Monad.IO.Class (liftIO)
import           KeyState (isPressed, isTouched, isHeld, isReleased)
import           System.FilePath        ((</>))

import           Game.Effect.Clock    (getGlobalTime)
import           Game.Effect.Input    (getInputs)
import           Game.Effect.Renderer (getWindowDims)

import           Game.World.TH         (ECS)
import           Game.Loaders.Save     (saveDataFile, loadDataFile)
import           Game.Loaders.Cfg      (readMapCfg, readMapMedia)
import           Game.System.Camera    (cameraEvents)
import           Game.System.Collision (stepCollisionSystem)
import           Game.Util.Constants   (frameDeltaSeconds, assetPath, shaderPath, objPath)
import           Game.Loaders.Program  (createProgram)
import           Game.Loaders.Obj.Loader  (loadObjFile)
import           Game.Util.BSP.Render  (BSPRenderData, renderBSP)
import           Game.Util.GLError     (printGLErrors)
import           Game.Util.CardinalDir
  (posX, posZ, negX, negZ, neutral, toDir)
import           Game.Util.Camera
  ( cameraViewMatrix
  , cameraProjectionMatrix
  , CameraEntity )
import           Game.Util.Move (runMoveCommand, Moveable)
import           Game.Types
  ( VideoConfig(..)
  , PlayerInput(..)
  , Inputs(..)
  , SceneControl(..)
  , PolygonMode(..)
  , PolygonModeType(..)
  , ProgramMap(..)

  , StaminaMeter(..)
  , SanityMeter(..)

  , HasMoveCommand(..)
  , MoveCommand(..)
  , Degrees(..)
  , Rotation(..)
  , Translation(..)

  , Camera(..)
  , ClippingPlanes(..)
  , FieldOfView(..)
  , Orientation(..)
  , CameraAxes(..)
  , Collider(..)
  , CollisionModule(..)

  , Player(..)
  , Facing(..)
  , CardinalDir(..)
  , Terrain
  , SimpleCube(..)

  , BSPMap
  , DebugHUD(..)
  , HUDInfo(..)
  , HUDType(..)
  , FontInfo(..)
  , FontMap(..)
  , ShaderInfo(..)
  , Position3D(..)
  , Scene(..)
  , RenderGlobals(..)
  )
import Game.System.Scratch.VAO (initVAO, cleanUpVAO)
import Game.System.Scratch.ColorCube
  ( ColorCube
  , PlayerCube
  , initColorCube
  , stepColorCube
  , drawColorCube )
import Game.System.Scratch.SimpleCube
  ( SimpleCubeProto
  , initCubeGLAttrs
  , drawSimpleCube
  )
import Game.System.Scratch.PlayerBillboard
  ( PlayerB
  , initPlayerBillboard
  , stepPlayerBillboard
  , createPlayerFrustum
  , destroyPlayerFrustum
  , drawPlayerBillboard )
import Game.System.Scratch.Terrain
  ( TerrainE
  , initTerrain
  , drawTerrain )
import Game.System.Scratch.DebugHUD
  ( HUDEntity
  , initDebugHUD
  , hudProgramName
  , loadFontMap
  , drawDebugHUD
  , stepDebugHUD
  )
import Game.System.Scratch.Billboard
  ( RenderBillboard
  , initBillboards
  , drawBillboard )
import Game.System.Scratch.BoundingBoxes
  ( makeProgram
  , bbProgramName
  , drawBoundingBox
  )
import Game.System.Scratch.PlayerModel
  ( PlayerModel(..)
  , initPlayerModel
  , drawPlayerModel
  )

initialize :: ECS ()
initialize = do
  -- init VAO
  initVAO
  -- attempt to render suzanne the monkey
  initPlayerModel
  -- terrain
  initTerrain
  -- entities
  initPlayerBillboard
  -- make simple cube program, add to program map
  cubeProgram <- liftIO $ makeProgram bbProgramName
  buf         <- liftIO $ initCubeGLAttrs
  -- add program to map
  pmap <- get global :: ECS ProgramMap
  set global (pmap { _programMap = insert bbProgramName (buf, cubeProgram) (_programMap pmap) })
  -- create some bland normal cubes
  let defaultCm   = CollisionModule { _collider     = BoxCollider (L.V3 1 1 1)
                                    , _hasCollision = False }
      defaultQuat = L.Quaternion 1 (L.V3 0 0 0)
      quatAt30deg = L.axisAngle (L.V3 0 1 0) (pi / 12)
      mov1        = (Orientation defaultQuat, Position3D $ L.V3   2  1 (-4))
      mov2        = (Orientation quatAt30deg, Position3D $ L.V3 (-2) 1 (-2))
  newEntity (SimpleCube, (buf, cubeProgram), mov1, defaultCm)
  newEntity (SimpleCube, (buf, cubeProgram), mov2, defaultCm)

  -- initialize HUD
  -- note, x & y positions are [-1/2 screen, 1/2 screen] both axes,
  -- 0,0 is the center
  let fpsInfo = FontInfo { _fText = "Fps: "
                         , _fxPos = 10
                         , _fyPos = 550
                         , _fSize = 0.5 }
      posInfo = FontInfo { _fText = "Pos: "
                         , _fxPos = 10
                         , _fyPos = 500
                         , _fSize = 0.5 }
      facInfo = FontInfo { _fText = "Facing: "
                         , _fxPos = 10
                         , _fyPos = 600
                         , _fSize = 0.5 }

  newEntity (FPSCounter,      fpsInfo, Position3D $ L.V3 10 550 0)
  newEntity (PositionTracker, posInfo, Position3D $ L.V3 10 500 0)
  newEntity (PlayerFacing,    facInfo, Position3D $ L.V3 10 600 0)

  (fbuf, fprog) <- liftIO $ initDebugHUD
  -- add program to map
  pmap' <- get global :: ECS ProgramMap
  set global (pmap' { _programMap = insert hudProgramName (fbuf, fprog) (_programMap pmap') })

  fontMap <- liftIO $ loadFontMap
  set global fontMap
  -- mapM_ newEntity hud

  -- camera
  (c, p, o) :: (Camera, Position3D, Orientation) <- liftIO $ loadDataFile "test.json"
  newEntity (c, (o, p))
  cmap $ \((_, m) :: CameraEntity) ->
    runMoveCommand (Move'Compose
      (Move'Translate $ Translation $ L.V3 0 2 4)
      (Move'Rotate      Pitch (Degrees (-15))) ) m
  return ()

playerEvents :: Inputs
             -> (Player, Moveable, Not HasMoveCommand)
             -> Either Player (Player, HasMoveCommand)
playerEvents inputs (p@(Player (Facing dir)), _, _) =
  let m            = _inputs . _keyboardInput $ inputs
      sHeld  = isHeld $ m ! SDL.KeycodeLShift
      toNegX = if isTouched $ m ! SDL.KeycodeA then negX else neutral
      toPosX = if isTouched $ m ! SDL.KeycodeD then posX else neutral
      toNegZ = if isTouched $ m ! SDL.KeycodeS then negZ else neutral
      toPosZ = if isTouched $ m ! SDL.KeycodeW then posZ else neutral
      t      = realToFrac frameDeltaSeconds :: Float
      total  = toNegX + toPosX + toNegZ + toPosZ
      dir'   = if sHeld then dir else toDir dir total
      total' = if sHeld then (total / 2) else total
      action = Move'Translate (Translation $ total' L.^* t)
  in if total == neutral
     then Left p
     else Right (Player (Facing dir'), HasMoveCommand action)

quitOnEsc :: Inputs -> ECS ()
quitOnEsc inputs = do
  let m = _inputs . _keyboardInput $ inputs
  when (isPressed $ m ! SDL.KeycodeEscape) $ do
    sc <- get global :: ECS SceneControl
    set global $ sc { _nextScene = Scene'Quit }

handlePlayerFrustum :: Inputs -> ECS ()
handlePlayerFrustum inputs = do
  let m = _inputs . _keyboardInput $ inputs
  when (isPressed  $ m ! SDL.KeycodeLShift) $ do
    cmapM_ createPlayerFrustum
  when (isReleased $ m ! SDL.KeycodeLShift) $ do
    cmapM_ destroyPlayerFrustum
  return ()

cleanUp :: ECS ()
cleanUp = do
  -- destroy the camera
  cmap $ \(_ :: CameraEntity  ) -> Not :: Not CameraEntity
  cmap $ \(_ :: HasMoveCommand) -> Not :: Not HasMoveCommand
  cleanUpVAO

step :: ECS ()
step = do
  stepDebugHUD
  stepCollisionSystem
  -- get inputs
  inputs <- getInputs
  -- if escape pressed, transition to quit
  quitOnEsc          inputs
  -- send player events based on WASD presses
  cmap $ playerEvents inputs
  -- on hold shift, create player frustum
  handlePlayerFrustum inputs
  -- rotate da cubes!!!
  cmap stepColorCube
  -- update da sprites!!!
  stepPlayerBillboard
  -- resolve movement based on movement events
  cmap $ \(HasMoveCommand e, c :: Moveable) ->
    (runMoveCommand e c, Not :: Not HasMoveCommand)


render :: ECS ()
render = do
  -- render cube
  vc <- get global :: ECS VideoConfig
  sm <- get global :: ECS ProgramMap
  fm <- get global :: ECS FontMap
  dims <- liftIO $ getWindowDims vc
  cmapM_ $ \(camera :: CameraEntity) -> do
    let camProjMatrix  = cameraProjectionMatrix dims camera
        camViewMatrix  = cameraViewMatrix camera
        mats           = (camProjMatrix, camViewMatrix)
        (_, (_, cPos)) = camera
        renderGlobals  = RenderGlobals { _rgCamera     = ( camProjMatrix
                                                         , camViewMatrix
                                                         , cPos )
                                       , _rgProgramMap = sm
                                       , _rgFontMap    = fm
                                       , _rgDims       = dims }
    cmapM_ $ \(r :: TerrainE)        ->
      liftIO $ drawTerrain renderGlobals r
    cmapM_ $ \(r :: PlayerModel)     ->
      liftIO $ drawPlayerModel renderGlobals r
    cmapM_ $ (drawPlayerBillboard mats)
    cmapM_ $ \(r :: (CollisionModule, Position3D)) ->
      liftIO $ drawBoundingBox renderGlobals r
    cmapM_ $ \(r :: HUDEntity) ->
      liftIO $ drawDebugHUD renderGlobals r
