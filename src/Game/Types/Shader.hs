-- {-# LANGUAGE TypeFamilies      #-}
-- {-# LANGUAGE DataKinds         #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}

module Game.Types.Shader where

import qualified Graphics.Rendering.OpenGL as GL

-- import           Game.Util.FBO (FBO(..))

-- data ShaderGalaxy t = ShaderGalaxy
--   { _shaderProgram :: ShaderProgram t
--   , _onDrawProgram :: t -> IO t
--   , _global        :: t }
--
-- data ShaderUniverse t = ShaderUniverse
--   { _galaxies    :: [ShaderGalaxy t]
--   , _postShaders :: [ShaderProgram FBO] }


data ShaderProgram t = ShaderProgram
  { _program    :: GL.Program
  , _attribs      :: [AttribGPU t]
  , _uniforms     :: [UniformGPU t] }
  -- , _shaderInfo :: (GLSLInfo t) -- used for logging

data UniformGPU t = UniformGPU
  { _uniformLocation :: GL.UniformLocation
  , _onBindUniform   :: t -> IO () }

data AttribGPU t = AttribGPU
  { _buffer         :: GL.BufferObject
  , _onBindAttrib   :: t -> IO ()
  , _attribLocation :: GL.AttribLocation
  , _descriptor     :: forall a. GL.VertexArrayDescriptor a
  , _length         :: GL.NumArrayIndices }


-- data Shader (p :: GL.ShaderType) t =
--     Shader   (ShaderTypeProxy p) (forall a. ShaderM p t a ())
--     FromBS   (ShaderTypeProxy p) (GLSLInfo t) B.ByteString
--   | FromFile (ShaderTypeProxy p) (GLSLInfo t) FilePath

-- data ShaderTypeProxy (t :: GL.ShaderType) = Proxy
--
-- class ShaderTypeVal a where
--     typeVal :: a -> GL.ShaderType
-- instance ShaderTypeVal (ShaderTypeProxy GL.VertexShader) where
--     typeVal = const GL.VertexShader
-- instance ShaderTypeVal (ShaderTypeProxy GL.TessControlShader) where
--     typeVal = const GL.TessControlShader
-- instance ShaderTypeVal (ShaderTypeProxy GL.TessEvaluationShader) where
--     typeVal = const GL.TessEvaluationShader
-- instance ShaderTypeVal (ShaderTypeProxy GL.GeometryShader) where
--     typeVal = const GL.GeometryShader
-- instance ShaderTypeVal (ShaderTypeProxy GL.FragmentShader) where
--     typeVal = const GL.FragmentShader
-- instance ShaderTypeVal (ShaderTypeProxy GL.ComputeShader) where
--     typeVal = const GL.ComputeShader
