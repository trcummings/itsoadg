module Game.Loaders.Cfg where

import System.FilePath ((</>))

import Game.Util.File      (withBinaryFile, readLines)
import Game.Util.Constants (assetPath)
import Game.Util.BSP.Read  (readBSP)
import Game.Types          (BSPMap)

data LevelModel =
  MapModel String
  deriving (Read, Show)

-- read in initial entities from .cfg
readMapCfg :: FilePath -> IO ()
readMapCfg path = withBinaryFile path $ \handle -> do
  lines' <- readLines handle
  print lines'
  return ()

readMapMedia :: FilePath -> IO BSPMap
readMapMedia path = withBinaryFile path $ \handle -> do
  lines' <- readLines handle
  print lines'
  let levelModels        = linesToLevelModels lines'
      (MapModel lvlName) = head levelModels
  bsp <- readBSP $ assetPath </> lvlName
  return bsp

linesToLevelModels :: [String] -> [LevelModel]
linesToLevelModels []         = []
linesToLevelModels (str:strs) = (read str) : (linesToLevelModels strs)
