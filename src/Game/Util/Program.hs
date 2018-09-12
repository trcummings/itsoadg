module Game.Util.Program
  ( createProgram
  , getUniform
  , getAttrib ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString           as B
import qualified Data.Map                  as Map (lookup)

import           SDL              (($=))
import           Control.Monad    (unless, mapM_)
import           System.IO        (hPutStrLn, stderr)
import           System.Exit      (exitFailure)
import           Data.List        (isSuffixOf)
import           Data.Map.Strict  (fromList)
import           Data.Maybe       (maybe)
-- import           System.Directory (doesFileExist)

import           Game.Types (ShaderInfo(..), ShaderProgram(..))


printError :: IO ()
printError = GL.get GL.errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

createProgram :: [ShaderInfo] -> IO ShaderProgram
createProgram shaderInfo = do
  shaders <- mapM loadShader $ shaderInfo
  program <- compileAndLink shaders
  -- get attributes, attach to the program
  (attribs, uniforms) <- getActives program
  -- attach all attributes to the program
  mapM_ (\(name, (loc, _)) -> GL.attribLocation program name $= loc) attribs
  return $ ShaderProgram (fromList attribs) (fromList uniforms) program
  -- -- let info@(GLSLInfo ins uniforms _) = gatherInfo shaderSequence
  -- -- compile the shader from the given information (path, shaderType)
  -- program <- compileAndLink shaders
  -- -- create list of AttribGPUs from the given info
  -- attrs   <- if   null ins
  --            then return []
  --            else createAttrList program global ins
  -- -- create list of UniformGPUs from the given info
  -- unis    <- if   null uniforms
  --            then return []
  --            else createUniformList program uniforms
  -- -- -- return our shader program
  -- return $ ShaderProgram { _glProgram  = program
  --                        , _attribs    = attrs
  --                        , _uniforms   = unis
  --                        , _shaderInfo = info }

loadShader :: ShaderInfo -> IO GL.Shader
loadShader (ShaderInfo sType path) =
  B.readFile path >>= compileShader sType

compileAndLink :: [GL.Shader] -> IO GL.Program
compileAndLink shaders = do
  -- create a program
  program <- GL.createProgram
  -- attach all shaders to the program
  mapM_ (GL.attachShader program) shaders
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
  -- detach the shaders now that our program is ok
  -- mapM_ (GL.detachShader program) shaders
  -- delete the shader pointers, all we need is the program
  GL.deleteObjectNames shaders
  -- return the program
  return program

  -- shaders <- compileAll shaderSeq
  -- program <- GL.createProgram
  -- --mapM_ (GL.attachShader program) shaders
  -- mapM_ (\s -> GL.attachShader program s >> GL.deleteObjectName s) shaders
  -- GL.linkProgram program
  -- return program
--
-- compileAll :: ShaderSequence t -> IO [GL.Shader]
-- compileAll (Wrapped current : rest) = do
--     firstPrograms <- compile current
--     otherPrograms <- compileAll rest
--     return $ firstPrograms : otherPrograms
-- compileAll [] = return []
--
-- compile :: ShaderTypeVal (ShaderTypeProxy p)
--         => Shader p t -> IO GL.Shader
-- compile (Shader proxy glsl) =
--     let code = generateGLSL glsl
--         shaderType = typeVal proxy
--     in compileShader shaderType code
-- compile (FromBS proxy _ code) =
--     let shaderType = typeVal proxy
--     in compileShader shaderType code
-- compile (FromFile proxy _ file) =
--     let shaderType = typeVal proxy
--     in compileShader shaderType =<<
--         B.readFile file

compileShader :: GL.ShaderType -> B.ByteString -> IO GL.Shader
compileShader shaderType source = do
  -- create the shader
  shader <- GL.createShader shaderType
  -- set shader source code byte string
  GL.shaderSourceBS shader $= source
  -- compile the shader
  GL.compileShader shader
  -- did the shader compile properly?
  ok <- GL.get $ GL.compileStatus shader
  -- if compilation failed, throw error, log failure, exit
  unless ok $ do
    shaderLog <- GL.get $ GL.shaderInfoLog shader
    putStrLn $ "Log:" ++ shaderLog
    printError
    exitFailure
  return shader

-- | Get all attributes and uniforms used by a program. Note that
-- unused parameters may be elided by the compiler, and so will not be
-- considered as active.
getActives :: GL.Program ->
              IO ( [(String, (GL.AttribLocation , GL.VariableType))]
                 , [(String, (GL.UniformLocation, GL.VariableType))] )
getActives p =
  (,) <$> (GL.get (GL.activeAttribs p)
            >>= mapM (aux (GL.attribLocation p)))
      <*> (GL.get (GL.activeUniforms p)
            >>= mapM (aux (GL.uniformLocation p) . on3 trimArray))
  where
    aux f (_,t,name) = GL.get (f name) >>= \l -> return (name, (l, t))
    on3 f (a,b,c) = (a, b, f c)
    -- An array uniform, foo, is sometimes given the name "foo" and
    -- sometimes the name "foo[0]". We strip off the "[0]" if present.
    trimArray n = if "[0]" `isSuffixOf` n
                  then take (length n - 3) n
                  else n

getUniform :: ShaderProgram -> String -> GL.UniformLocation
getUniform sp n = maybe (error msg) fst . Map.lookup n $ _uniforms sp
  where msg = "Uniform " ++ show n ++ " is not active"

getAttrib :: ShaderProgram -> String -> GL.AttribLocation
getAttrib sp n = maybe (error msg) fst . Map.lookup n $ _attribs sp
  where msg = "Attrib " ++ show n ++ " is not active"

-- gatherInfo :: ShaderSequence t -> GLSLInfo t
-- gatherInfo (Wrapped (Shader proxy glsl) : shaders) =
--     let GLSLInfo ins uniforms outs = evalShaderM glsl
--         ins' = case typeVal proxy of
--             GL.VertexShader -> ins
--             _               -> []
--         outs' = case typeVal proxy of
--             GL.FragmentShader -> outs
--             _                 -> []
--     in GLSLInfo ins' uniforms outs' <> gatherInfo shaders
-- gatherInfo (Wrapped (FromBS proxy info _) : shaders) =
--     let GLSLInfo ins uniforms outs = info
--         (ins', outs') = case typeVal proxy of
--             GL.VertexShader -> (ins, outs)
--             _               -> ([], [])
--     in GLSLInfo ins' uniforms outs' <> gatherInfo shaders
-- gatherInfo (Wrapped (FromFile proxy info _) : shaders) =
--     let GLSLInfo ins uniforms outs = info
--         (ins', outs') = case typeVal proxy of
--             GL.VertexShader -> (ins, outs)
--             _               -> ([], [])
--     in GLSLInfo ins' uniforms outs' <> gatherInfo shaders
-- gatherInfo [] = mempty
