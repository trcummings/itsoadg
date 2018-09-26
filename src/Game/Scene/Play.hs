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
import           Data.Map ((!), keys)
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

import           Game.World.TH        (ECS)
import           Game.Loaders.Save    (saveDataFile, loadDataFile)
import           Game.Loaders.Cfg     (readMapCfg, readMapMedia)
import           Game.System.Camera   (cameraEvents)
import           Game.Util.Constants  (frameDeltaSeconds, assetPath, shaderPath)
import           Game.Loaders.Program (createProgram)
import           Game.Util.BSP.Render (BSPRenderData, renderBSP)
import           Game.Util.GLError    (printGLErrors)
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

  , Player(..)
  , Facing(..)
  , CardinalDir(..)
  , Terrain

  , BSPMap
  , DebugHUD(..)
  , HUDInfo(..)
  , ShaderInfo(..)
  , Position3D(..)
  , Scene(..) )
import Game.System.Scratch.VAO (initVAO, cleanUpVAO)
import Game.System.Scratch.ColorCube
  ( ColorCube
  , PlayerCube
  , initColorCube
  , stepColorCube
  , drawColorCube )
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
  ( DebugHUDEntity
  , initDebugHUD
  , drawDebugHUD
  , stepDebugHUD )
import Game.System.Scratch.Billboard
  ( RenderBillboard
  , initBillboards
  , drawBillboard )


initialize :: ECS ()
initialize = do
  -- init VAO
  initVAO
  -- -- load config data
  -- liftIO $ readMapCfg $ assetPath </> "leveleg.cfg"
  -- bsp     <- liftIO $ readMapMedia $ assetPath </> "leveleg.med"
  -- liftIO $ printGLErrors "initialize make bsp"
  -- program <- liftIO $ createProgram
  --               [ ShaderInfo GL.VertexShader   (shaderPath </> "bsp.v.glsl")
  --               , ShaderInfo GL.FragmentShader (shaderPath </> "bsp.f.glsl") ]
  -- liftIO $ printGLErrors "initialize make bsp program"
  -- -- create BSP entity
  -- newEntity (bsp, program)
  -- terrain
  initTerrain
  -- entities
  -- initColorCube
  -- initTextureCube
  initPlayerBillboard
  -- create all billboards
  -- billboards <- liftIO $ initBillboards
  -- mapM_ newEntity billboards
  -- initialize HUD
  hud <- liftIO $ initDebugHUD
  mapM_ newEntity hud

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
  --     rt = if leftPress && rightPress
  --          then Nothing
  --          else if leftPress
  --               then Just $ toDolly $ L.V3 (-1) 0 0
  --               else if rightPress
  --                    then Just $ toDolly $ L.V3 1 0 0
  --                    else Nothing
  --     vx = if forwardPress && backPress
  --          then Nothing
  --          else if backPress
  --               then Just $ toDolly $ L.V3 0 0 1
  --               else if forwardPress
  --                    then Just $ toDolly $ L.V3 0 0 (-1)
  --                    else Nothing
  -- in case (vx, rt) of
  --     (Nothing , Nothing ) -> Left p
  --     (Just vx', Just rt') -> Right (p, HasMoveCommand (rt' + vx'))
  --     (Just vx', _       ) -> Right (p, HasMoveCommand vx')
  --     (_       , Just rt') -> Right (p, HasMoveCommand rt')

quitOnEsc :: Inputs -> ECS ()
quitOnEsc inputs = do
  let m = _inputs . _keyboardInput $ inputs
  when (isPressed $ m ! SDL.KeycodeEscape) $ do
    sc <- get global :: ECS SceneControl
    set global $ sc { _nextScene = Scene'Quit }

toggleWireframeOnP :: Inputs -> ECS ()
toggleWireframeOnP inputs = do
  let m = _inputs . _keyboardInput $ inputs
  when (isPressed $ m ! SDL.KeycodeP) $ do
    PolygonMode pm <- get global
    case pm of
      PolygonMode'Normal    -> do
        liftIO $ GL.polygonMode $= (GL.Line, GL.Line)
        set global (PolygonMode PolygonMode'Wireframe)
      PolygonMode'Wireframe -> do
        liftIO $ GL.polygonMode $= (GL.Fill, GL.Fill)
        set global (PolygonMode PolygonMode'Normal)

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
  -- destroy the cube
  -- FIXME: need to free the Model resources
  -- cmap $ \(_ :: Model, _ :: Position3D) -> Not :: Not (Model, Position3D)
  -- destroy the camera
  cmap $ \(_ :: CameraEntity  ) -> Not :: Not CameraEntity
  cmap $ \(_ :: HasMoveCommand) -> Not :: Not HasMoveCommand
  cleanUpVAO

step :: ECS ()
step = do
  stepDebugHUD
  -- get inputs
  inputs <- getInputs
  -- if escape pressed, transition to quit
  quitOnEsc          inputs
  -- toggle wireframe on press "p"
  toggleWireframeOnP inputs
  -- send player events based on WASD presses
  -- cmap $ cameraEvents inputs
  cmap $ playerEvents inputs
  -- on hold shift, create player frustum
  handlePlayerFrustum inputs
  -- rotate da cubes!!!
  cmap stepColorCube
  -- update da sprites!!!
  cmap stepPlayerBillboard
  -- resolve movement based on movement events
  cmap $ \(HasMoveCommand e, c :: Moveable) ->
    (runMoveCommand e c, Not :: Not HasMoveCommand)
  -- orbit camera around player
  -- cmapM_ $ \(Player, _ :: Not HasMoveCommand, Position3D pPos) -> do
  --   cmap $ \(cam :: Camera, (Orientation cOr, Position3D cPos) :: Moveable) ->
  --     ( cam, (Orientation pOr', Position3D cPos) )
  --   return (Player, pMov', Not :: Not HasMoveCommand)

render :: ECS ()
render = do
  -- render cube
  vc <- get global :: ECS VideoConfig
  dims <- liftIO $ getWindowDims vc
  cmapM_ $ \(camera :: CameraEntity) -> do
    let camProjMatrix  = cameraProjectionMatrix dims camera
        camViewMatrix  = cameraViewMatrix camera
        mats           = (camProjMatrix, camViewMatrix)
        (_, (_, cPos)) = camera
    -- cmapM_ $ \(r :: BSPRenderData) -> liftIO $ renderBSP mats cPos r
    cmapM_ $ \(r :: TerrainE)        -> liftIO $ drawTerrain mats r
    -- cmapM_ $ \(r :: RenderBillboard) -> liftIO $ drawBillboard mats r
    -- cmapM_ $ \(r :: ColorCube)  -> liftIO $ drawColorCube mats r
    -- cmapM_ $ \(r :: PlayerCube) -> liftIO $ drawColorCube mats r
    -- cmapM_ $ \(r :: TexCube)       -> liftIO $ drawTextureCube     mats r
    cmapM_ $ (drawPlayerBillboard mats)
    return ()
  cmapM_ $ \(hud@(dHud, _, _) :: DebugHUDEntity) -> do
    let (HUDInfo dMap) = _hudInfo dHud
    liftIO $ mapM_ (drawDebugHUD hud dims) $ Map.keys dMap
