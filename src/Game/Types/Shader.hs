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

-- data UniformGPU t = UniformGPU
--   { _uniformLocation :: GL.UniformLocation
--   , _onBindUniform   :: t -> IO () }

-- data AttribGPU = AttribGPU
--   { _buffer         :: GL.BufferObject
--   -- , _onBindAttrib   :: t -> IO ()
--   , _attribLocation :: GL.AttribLocation
--   , _descriptor     :: forall a. GL.VertexArrayDescriptor a
--   , _length         :: GL.NumArrayIndices }
