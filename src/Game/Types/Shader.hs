{-# LANGUAGE Rank2Types #-}

module Game.Types.Shader where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString           as B
import qualified Linear                    as L
import           Data.Map (Map)

-- import           Game.Util.FBO (FBO(..))

-- data ShaderGalaxy t = ShaderGalaxy
--   { _shaderProgram :: ShaderProgram t
--   , _onDrawProgram :: t -> IO t
--   , _global        :: t }
--
-- data ShaderUniverse t = ShaderUniverse
--   { _galaxies    :: [ShaderGalaxy t]
--   , _postShaders :: [ShaderProgram FBO] }

data ShaderInfo = ShaderInfo GL.ShaderType FilePath

data ShaderProgram = ShaderProgram
  { _attribs   :: Map String (GL.AttribLocation,  GL.VariableType)
  , _uniforms  :: Map String (GL.UniformLocation, GL.VariableType)
  , _glProgram :: GL.Program }

-- data ShaderProgram t = ShaderProgram
--   { _glProgram  :: GL.Program
--   , _attribs    :: [AttribGPU t]
--   , _uniforms   :: [UniformGPU t] }
--   -- , _shaderInfo :: GLSLInfo t }-- used for logging

-- data UniformGPU t = UniformGPU
--   { _uniformLocation :: GL.UniformLocation
--   , _onBindUniform   :: t -> IO () }

-- data AttribGPU = AttribGPU
--   { _buffer         :: GL.BufferObject
--   -- , _onBindAttrib   :: t -> IO ()
--   , _attribLocation :: GL.AttribLocation
--   , _descriptor     :: forall a. GL.VertexArrayDescriptor a
--   , _length         :: GL.NumArrayIndices }

-- data GLSLInfo t = GLSLInfo [In t] [Uniform t] [Out]
--
-- instance Monoid (GLSLInfo t) where
--   mempty = GLSLInfo [] [] []
--   mappend (GLSLInfo in1 uni1 out1) (GLSLInfo in2 uni2 out2) =
--     GLSLInfo (in1 ++ in2) (uni1 ++ uni2) (out1 ++ out2)
--
-- type Name = B.ByteString
--
-- data In t =
--     InFloat (t -> [GL.GLfloat])       Name
--   | InInt   (t -> [Int])              Name
--   | InVec2  (t -> [L.V2 GL.GLfloat])  Name
--   | InVec3  (t -> [L.V3 GL.GLfloat])  Name
--   | InVec4  (t -> [L.V4 GL.GLfloat])  Name
--   | InNone                            Name
--   -- Unlikely, but...
--   | InMat4  (t -> [L.M44 GL.GLfloat]) Name
--
-- data Sampler2D = Sampler2DInfo GL.TextureObject GL.TextureUnit
--
-- data Uniform t =
--     UniformFloat     (t -> GL.GLfloat)       Name
--   | UniformInt       (t -> Int)              Name
--   | UniformVec2      (t -> L.V2  GL.GLfloat) Name
--   | UniformVec3      (t -> L.V3  GL.GLfloat) Name
--   | UniformVec4      (t -> L.V4  GL.GLfloat) Name
--   | UniformMat4      (t -> L.M44 GL.GLfloat) Name
--   | UniformSampler2D (t -> Sampler2D)        Name
--
-- data Out =
--     OutFloat     Name
--   | OutInt       Name
--   | OutBool      Name
--   | OutVec2      Name
--   | OutVec3      Name
--   | OutVec4      Name
--   | OutMat4      Name
--   | OutSampler2D Name
--   | OutNone      Name
--   deriving (Show, Eq)
