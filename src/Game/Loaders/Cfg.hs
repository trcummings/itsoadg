module Game.Loaders.Cfg where

import Game.Util.File (withBinaryFile, readLines)

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

linesToLevelModels :: [String] -> [LevelModel]
linesToLevelModels []         = []
linesToLevelModels (str:strs) = (read str) : (linesToLevelModels strs)
