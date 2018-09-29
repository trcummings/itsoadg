module Game.System.Scratch.BoundingBoxes where


import qualified Graphics.GLUtil           as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           SDL                    (($=))
import           Control.Monad          (mapM_)
import           Control.Monad.IO.Class (liftIO)
import           System.FilePath        ((</>))
import           Data.Map               ((!))
import           Control.Applicative
import           Apecs

import           Game.World.TH            (ECS)
import           Game.Util.Move           (Moveable)
import           Game.Util.Constants      (shaderPath)
import           Game.Util.BufferObjects  (fromSource)
import           Game.Loaders.Program     (createProgram, getUniform, getAttrib)
import           Game.Types
  ( Orientation(..)
  , Position3D(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , ShaderProgram(..)
  , SimpleCube(..)
  , ProjectionMatrix(..)
  , ViewMatrix(..)
  , CollisionModule(..)
  , Collider(..)
  , ProgramMap(..)
  , ProgramName(..)
  )

type CamInfo = (ProjectionMatrix, ViewMatrix, Position3D)
data RenderGlobals = RenderGlobals
  { _rgCamera     :: CamInfo
  , _rgProgramMap :: ProgramMap }

_cPos  :: CamInfo -> Position3D
_cPos  (_, _, x) = x
_cPMat :: CamInfo -> ProjectionMatrix
_cPMat (x, _, _) = x
_cVMat :: CamInfo -> ViewMatrix
_cVMat (_, x, _) = x

makeProgram :: ProgramName -> IO ShaderProgram
makeProgram (ProgramName name) = do
  let vertexShader   = shaderPath </> (name ++ ".v.glsl")
      fragmentShader = shaderPath </> (name ++ ".f.glsl")
  createProgram [ ShaderInfo GL.VertexShader   vertexShader
                , ShaderInfo GL.FragmentShader fragmentShader ]

toScalingMatrix :: L.V3 Float -> L.M44 Float
toScalingMatrix (L.V3 x y z) = L.V4
  (L.V4 x 0 0 0)
  (L.V4 0 y 0 0)
  (L.V4 0 0 z 0)
  (L.V4 0 0 0 1)



bbProgramName :: ProgramName
bbProgramName = ProgramName "simple_cube"

drawBoundingBox :: RenderGlobals
                -> (CollisionModule, Position3D)
                -> IO ()
drawBoundingBox globals (cm, Position3D mPos) = do
  let BoxCollider dims = _collider cm
      viewMatrix       = (_viewMatrix . _cVMat . _rgCamera) globals
      projMatrix       = (_projMatrix . _cPMat . _rgCamera) globals
      scaleMatrix      = toScalingMatrix
      modelMatrix      =
        L.mkTransformationMat L.identity mPos
        L.!*! toScalingMatrix (dims / 2)
      (buf, sProgram)  = (_programMap . _rgProgramMap ) globals ! bbProgramName
      posLoc           = getAttrib  sProgram "VertexPosition_modelspace"
      mLoc             = getUniform sProgram "ModelMatrix"
      vLoc             = getUniform sProgram "ViewMatrix"
      pLoc             = getUniform sProgram "ProjMatrix"
      cLoc             = getUniform sProgram "BoxColor"
  -- set current program to shaderProgram
  GL.currentProgram             $= Just (_glProgram sProgram)
  -- enable attribs
  GL.vertexAttribArray   posLoc $= GL.Enabled
  -- transform mvp uniform
  modelMatrix `U.asUniform` mLoc
  viewMatrix  `U.asUniform` vLoc
  projMatrix  `U.asUniform` pLoc
  -- box color
  let boxColor = if   _hasCollision cm
                 then L.V3 0 1 0 :: L.V3 Float
                 else L.V3 1 0 0 :: L.V3 Float
  boxColor `U.asUniform` cLoc
  -- bind to vertex buffer VB
  GL.bindBuffer GL.ArrayBuffer  $= (_vertexBuffer buf)
  GL.vertexAttribPointer posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )
  -- draw as lines
  GL.polygonMode $= (GL.Line, GL.Line)
  -- draw vertex arrays
  GL.drawArrays GL.Triangles 0 36
  -- reset to fill mode
  GL.polygonMode $= (GL.Fill, GL.Fill)
  -- disable all attributes
  GL.vertexAttribArray   posLoc $= GL.Disabled
  -- unset current program
  GL.currentProgram             $= Nothing
  return ()
