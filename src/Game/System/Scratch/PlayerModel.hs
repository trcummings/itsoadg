{-# LANGUAGE NamedFieldPuns #-}

module Game.System.Scratch.PlayerModel where

import qualified Graphics.GLUtil           as U
import qualified Graphics.GLUtil.Camera3D  as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import qualified Animate                   as A
import qualified Data.Vector               as V
import qualified Codec.Wavefront           as W
import           Linear                  ((!*!))
import           SDL                     (($=))
import           Foreign.Ptr             (nullPtr)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad           (mapM_, when, filterM)
import           Control.Monad.Extra     (partitionM)
import           Control.Lens            ((^.))
import           System.FilePath         ((</>))
import           Data.Map                (keys)
import           Data.Maybe              (maybe)
import           Apecs

import           Game.World.TH           (ECS)
import           Game.Util.Constants     (objPath, shaderPath)
import           Game.Loaders.Program    (createProgram, getAttrib, getUniform)
import           Game.Loaders.Texture    (getAndCreateTexture)
import           Game.Util.GLError       (printGLErrors)
import           Game.Util.Move          (Moveable)
import           Game.Util.Sprite        (loadSpriteSheet)
import           Game.Util.Camera        (_cPos, _cPMat, _cVMat)
import           Game.Util.BufferObjects (fromSource, replaceBuffer)
import           Game.Util.CardinalDir   (fromDir, toOpposingDir, toRadAngle)
import           Game.Types
  ( ProjectionMatrix(..)
  , ViewMatrix(..)
  , Position3D(..)
  , Orientation(..)
  , CollisionModule(..)
  , Collider(..)
  , ShaderProgram(..)
  , BufferResource(..)
  , ShaderInfo(..)
  , Player(..)
  , CardinalDir(..)
  , Facing(..)
  , Frustum(..)
  , FloorCircle(..)
  , Seconds(..)
  , Step(..)
  , RenderGlobals(..)
  )

type PlayerModel =
  ( Player
  , ShaderProgram
  , BufferResource
  , Moveable
  , CollisionModule
  , W.WavefrontOBJ
  )

defaultBufferResource :: BufferResource
defaultBufferResource = BufferResource Nothing Nothing Nothing Nothing Nothing

locToPos :: W.Location -> L.V3 Float
locToPos loc = L.V3 (W.locX loc) (W.locY loc) (W.locZ loc)

nrmToPos :: W.Normal -> L.V3 Float
nrmToPos nor = L.V3 (W.norX nor) (W.norY nor) (W.norZ nor)

fIdxToVec :: L.V3 (Maybe Int) -> Maybe (L.V3 Int)
fIdxToVec (L.V3 i1 i2 i3) =
  case i1 of Nothing  -> Nothing
             Just i1' -> case i2 of Nothing  -> Nothing
                                    Just i2' -> case i3 of Nothing  -> Nothing
                                                           Just i3' -> Just $ L.V3 i1' i2' i3'

makeIndices :: [W.Face] -> ([L.V3 Int], [L.V3 Int]) -> ([L.V3 Int], [L.V3 Int])
makeIndices []    (elems1, elems2) = (elems1, elems2)
makeIndices faces (elems1, elems2) =
  let (face : rest)        = faces
      W.Face fi1 fi2 fi3 _ = face
      fv                   = L.V3 fi1 fi2 fi3
      v                    = [W.faceLocIndex <$> fv]
      vn                   =
        case (fIdxToVec $ W.faceNorIndex <$> fv) of
          Nothing  -> []
          Just vn' -> [vn']
  in makeIndices rest (elems1 ++ v, elems2 ++ vn)

initPlayerModel :: ECS ()
initPlayerModel = do
  let vertexShader   = shaderPath  </> "suzanne.v.glsl"
      fragmentShader = shaderPath  </> "suzanne.f.glsl"
  -- load in shaders
  program <-
    liftIO $ createProgram [ ShaderInfo GL.VertexShader   vertexShader
                           , ShaderInfo GL.FragmentShader fragmentShader ]
  -- load obj data
  obj  <- liftIO $ W.fromFile $ objPath </> "suzanne.obj"
  case obj of
    Left  a -> liftIO $ putStrLn $ show a
    Right wObj -> do
      let verts    = V.toList $ V.map locToPos  $ W.objLocations wObj
          norms    = V.toList $ V.map nrmToPos  $ W.objNormals   wObj
          elems    = V.toList $ V.map W.elValue $ W.objFaces     wObj
          (e1, e2) = flip makeIndices ([],[]) elems
          idxes = e1 ++ e2
      -- create the buffer related data
      vb  <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) $ verts
      nm  <- liftIO $ fromSource (GL.StaticDraw, GL.ArrayBuffer) $ norms
      is  <- liftIO $ fromSource (GL.StaticDraw, GL.ElementArrayBuffer) $ idxes
      -- define the entity
      newEntity
        ( Player (Facing CardinalDir'South)
        , program
        , defaultBufferResource { _vertexBuffer   = Just vb
                                , _normalBuffer   = Just nm
                                , _indexBuffer    = Just is }
        , ( Orientation $ L.Quaternion 1 (L.V3 0 0 0)
          , Position3D  $ L.V3 0 1 0 )
        , CollisionModule { _collider     = BoxCollider (L.V3 1 1 1)
                          , _hasCollision = False }
        , wObj
        )
      return ()


drawPlayerModel :: RenderGlobals -> PlayerModel -> IO ()
drawPlayerModel globals (_, sProgram, br, (Orientation o, Position3D mPos), _, obj) = do
      -- vertex shader
  let viewMatrix  = (_viewMatrix . _cVMat . _rgCamera) globals
      projMatrix  = (_projMatrix . _cPMat . _rgCamera) globals
      posLoc      = getAttrib  sProgram "v_coord"
      nrmLoc      = getAttrib  sProgram "v_normal"
      mLoc        = getUniform sProgram "ModelMatrix"
      vLoc        = getUniform sProgram "ViewMatrix"
      pLoc        = getUniform sProgram "ProjMatrix"
      iLoc        = getUniform sProgram "m_3x3_inv_transp"
      ivLoc       = getUniform sProgram "v_inv"

      modelMatrix = L.mkTransformation o mPos
      invTrans    = L.transpose $ L.inv33 $ modelMatrix ^. L._m33
      vInv        = L.inv33 $ viewMatrix ^. L._m33

  -- set current program to shaderProgram
  GL.currentProgram             $= Just (_glProgram sProgram)

  -- enable attribs
  GL.vertexAttribArray   posLoc $= GL.Enabled
  GL.vertexAttribArray   nrmLoc $= GL.Enabled

  -- uniforms
  invTrans    `U.asUniform` iLoc
  vInv        `U.asUniform` ivLoc
  modelMatrix `U.asUniform` mLoc
  viewMatrix  `U.asUniform` vLoc
  projMatrix  `U.asUniform` pLoc

  -- vertices
  GL.bindBuffer GL.ArrayBuffer  $= (_vertexBuffer br)
  GL.vertexAttribPointer posLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )

  -- normals
  GL.bindBuffer GL.ArrayBuffer  $= (_normalBuffer br)
  GL.vertexAttribPointer nrmLoc $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0 )

  -- bind index buffer
  GL.bindBuffer GL.ElementArrayBuffer $= (_indexBuffer br)

  -- draw vertex arrays
  let numTris = fromIntegral $ 9 * (V.length $ W.objFaces obj)
  GL.drawElements GL.Triangles numTris GL.UnsignedInt nullPtr

  -- unbind index buffer
  GL.bindBuffer GL.ElementArrayBuffer $= Nothing

  -- disable all attributes
  GL.vertexAttribArray   posLoc $= GL.Disabled
  GL.vertexAttribArray   nrmLoc $= GL.Disabled
  -- unset current program
  GL.currentProgram             $= Nothing
