module Game.Util.Shaders where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import           SDL (($=))
import           Control.Monad (unless, mapM_)
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)

data VertexShaderPath   = VertexShaderPath   FilePath
data FragmentShaderPath = FragmentShaderPath FilePath

printError :: IO ()
printError = GL.get GL.errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

makeShader :: GL.ShaderType -> B.ByteString -> IO GL.Shader
makeShader shaderType source = do
  -- create the shader
  shader <- GL.createShader shaderType
  -- set shader source code byte string
  GL.shaderSourceBS shader $= source
  -- compile the shader
  GL.compileShader shader
  -- did the shader compile properly?
  shaderOk <- GL.get $ GL.compileStatus shader
  -- if compilation failed, throw error, log failure, exit
  unless shaderOk $ do
    shaderLog <- GL.get $ GL.shaderInfoLog shader
    putStrLn $ "Log:" ++ shaderLog
    printError
    exitFailure
  return shader

-- a program represents fully processed executable OpenGL code
makeProgram :: [GL.Shader] -> [(String, GL.AttribLocation)] -> IO GL.Program
makeProgram shaders attributes = do
  -- create a program
  program <- GL.createProgram
  -- attach all shaders to the program
  mapM_ (GL.attachShader program) shaders
  -- attach all attributes to the program
  mapM_ (\(name, loc) -> GL.attribLocation program name $= loc) attributes
  -- link the program
  GL.linkProgram program
  -- did the program link properly?
  programOk <- GL.get $ GL.linkStatus program
  -- validate the program
  GL.validateProgram program
  -- did the program validate properly?
  status <- GL.get $ GL.validateStatus program
  -- if link or validation failed, throw error, log failure, exit
  unless (programOk && status) $ do
    programLog <- GL.get $ GL.programInfoLog program
    putStrLn programLog
    printError
    exitFailure
  return program

bindVBO :: GL.BufferObject
        -> GL.VertexArrayDescriptor a
        -> GL.AttribLocation
        -> IO ()
bindVBO vbo dsc loc = do
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  GL.vertexAttribPointer loc   $= (GL.ToFloat, dsc)
  GL.vertexAttribArray loc     $= GL.Enabled
