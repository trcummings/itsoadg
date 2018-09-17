module Game.Loaders.File where

import Control.Exception (bracket)
import System.IO
  ( IOMode(ReadMode)
  , Handle
  , hClose
  , hGetLine
  , hIsEOF
  , openBinaryFile )

withBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile path = bracket (openBinaryFile path ReadMode) hClose

readLines :: Handle -> IO [String]
readLines handle = do
  eof <- hIsEOF handle
  if eof
  then return []
  else do
    line   <- hGetLine  handle
    lines' <- readLines handle
    return $ line : lines'
