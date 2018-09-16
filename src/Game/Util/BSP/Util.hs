module Game.Util.BSP.Util where

import Foreign.Ptr           (Ptr, castPtr, plusPtr)
import Foreign.Storable      (Storable, sizeOf, peek)
import Foreign.Marshal.Array (peekArray)
import Foreign.C.Types       (CInt, CFloat)
import System.IO.Error       (mkIOError, eofErrorType)
import System.IO             (Handle, hGetBuf)
import Data.Typeable         (Typeable)
import Data.Bits             (testBit)
import Data.Word             (Word8)
import Data.List             ((!!))
import Control.Monad         (when)

getAndPeek :: (Storable a, Typeable a) => Handle -> Ptr a -> a -> IO a
getAndPeek handle buf be = do
   bytesRead <- hGetBuf handle buf (sizeOf be)
   when (bytesRead /= sizeOf be) $
      ioError $ mkIOError eofErrorType "hGetBufFully" (Just handle) Nothing
   peek buf

getAndPeeks :: (Storable a, Typeable a) =>
   Handle -> Ptr a -> a -> Int -> IO [a]
getAndPeeks handle buf be i =
   mapM (\_ -> getAndPeek handle buf be) [1..i]

getOffsets :: Int -> Int -> Int -> IO [Int]
getOffsets length' offset size = return $
  map ((offset+) . (size*)) [0..((length' `div` size) - 1)]

toInts :: Integral a => [a] -> [Int]
toInts = map fromIntegral

toFloats :: Real a => [a] -> [Float]
toFloats = map realToFrac

get2t :: [a] -> (a, a)
get2t list = (list !! 0, list !! 1)

get3t :: [a] -> (a, a, a)
get3t list = (list !! 0, list !! 1, list !! 2)

get4t :: [a] -> (a, a, a, a)
get4t list = (list !! 0, list !! 1, list !! 2, list !! 3)

toBools :: [Word8] -> [Bool]
toBools list = [ y | x <- list, y <- map (testBit x) [0..7]]

getInts :: Ptr a -> Int -> IO [Int]
getInts ptr n = do
 ints <- peekArray n (castPtr ptr :: Ptr CInt)
 return $ toInts ints

getFloats :: Ptr a -> Int -> IO [Float]
getFloats ptr n = do
 floats <- peekArray n (castPtr ptr :: Ptr CFloat)
 return $ toFloats floats

cIntSize :: Int
cIntSize = sizeOf (undefined :: CInt)

getPtrs :: Ptr a -> Int -> Int -> [Ptr a]
getPtrs ptr length' sze =
  map (plusPtr ptr . (sze *)) [0.. ((length' `div` sze) - 1)]
