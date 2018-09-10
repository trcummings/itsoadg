module Game.Loaders.Cfg where

import Control.Exception (bracket)
import System.IO
  ( IOMode(ReadMode)
  , Handle
  , hClose
  , hGetLine
  , hIsEOF
  , openBinaryFile )

data LevelModel =
  MapModel String
  deriving (Read, Show)

-- read in initial entities from .cfg
readMapCfg :: FilePath -> IO ()
readMapCfg path = withBinaryFile path $ \handle -> do
  lines' <- readLines handle
  print lines'
  return ()

readMapMedia :: FilePath -> IO ()
readMapMedia path = withBinaryFile path $ \handle -> do
  lines' <- readLines handle
  print lines'
  let levelModels        = linesToLevelModels lines'
      (MapModel lvlName) = head levelModels
  putStrLn lvlName
  return ()

readLines :: Handle -> IO [String]
readLines handle = do
  eof <- hIsEOF handle
  if eof
  then return []
  else do
    line   <- hGetLine  handle
    lines' <- readLines handle
    return $ line : lines'

withBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile path = bracket (openBinaryFile path ReadMode) hClose

linesToLevelModels :: [String] -> [LevelModel]
linesToLevelModels []         = []
linesToLevelModels (str:strs) = (read str) : (linesToLevelModels strs)
