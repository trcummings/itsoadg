module Game.Util.BSP.BitSet where

import Data.Array.IO
  ( IOUArray
  , newArray
  , writeArray
  , readArray
  , getBounds
  , rangeSize )

import Game.Util.BSP.Types (BitSet(..))

emptyBS :: Int -> IO BitSet
emptyBS size = BitSet <$> newArray (0, size-1) False

clearBS :: BitSet -> Int -> IO ()
clearBS (BitSet bs) i = writeArray bs i False

clearAllBS :: BitSet -> IO ()
clearAllBS bs = sizeBS bs >>= \size -> mapM_ (clearBS bs) [0 .. size - 1]

setBS :: BitSet -> Int -> IO ()
setBS (BitSet bs) i = writeArray bs i True

isSetBS :: BitSet -> Int -> IO Bool
isSetBS (BitSet bs) = readArray bs

sizeBS :: BitSet -> IO Int
sizeBS (BitSet bs) = fmap rangeSize (getBounds bs)
