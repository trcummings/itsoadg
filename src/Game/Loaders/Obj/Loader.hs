module Game.Loaders.Obj.Loader (loadObjFile) where

import Data.ByteString.Char8 as B (readFile)
import Control.Monad ((<=<))

import Game.Loaders.Obj.Types     (ObjData)
import Game.Loaders.Obj.Parser    (runObjParser)
import Game.Loaders.Obj.Processor (pieceTogether)

loadObjFile :: FilePath -> IO ObjData
loadObjFile path = do
  bs <- B.readFile path
  return $ pieceTogether . runObjParser $ bs
