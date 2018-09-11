module Game.Util.TGA where

import Data.Word (Word8)
import Control.Exception (bracket)
import System.IO (Handle, IOMode(ReadMode), openBinaryFile, hGetBuf, hClose)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (plusPtr, Ptr())
import Graphics.Rendering.OpenGL
  ( Size(..)
  , PixelData(..)
  , DataType(UnsignedByte)
  , PixelFormat (RGBA, RGB) )

import Game.Util.File (withBinaryFile)

-- reads a *.tga file
readTga :: FilePath -> IO (Maybe (Size, PixelData Word8))
readTga filePath =
   withBinaryFile filePath $ \handle -> do
   buf <- mallocBytes 6 :: IO (Ptr Word8)
   --the first 12 bytes of the header aren't used
   hGetBuf handle buf 6
   hGetBuf handle buf 6
   hGetBuf handle buf 6
   header <- peekArray 6 buf
   let w1       = (fromIntegral (header !! 1)) * 256 :: Int
       h1       = (fromIntegral (header !! 3)) * 256 :: Int
       width    = w1 + (fromIntegral (header !! 0))
       height   = h1 + (fromIntegral (header !! 2))
       bitspp   = (fromIntegral (header !! 4))
       numBytes = (bitspp `div` 8) * width * height
   --allocate memory for the image
   image <- mallocBytes numBytes
   hGetBuf handle image numBytes
   --define whether the pixels are in RGB or RGBA format.
   pixelFormat <- getFormat (fromIntegral bitspp)
   free buf
   --convert the pixels which are in BGR/BGRA to RGB/RGBA
   swapBytes' image (bitspp `div` 8) (width * height)
   putStrLn ("loaded " ++ filePath)
   return $ Just ( Size (fromIntegral width) (fromIntegral height)
                 , PixelData pixelFormat UnsignedByte image )

-- converts the image from bgr/bgra to rgb/rgba
-- perhaps the opengl bgra extension could be
-- used to avoid this
swapBytes' :: Ptr Word8 -> Int -> Int -> IO ()
swapBytes' image bytespp size =
   case bytespp of
      3 -> do mapM_ (swapByteRGB  . (plusPtr image) . (bytespp *)) [0..(size - 1)]
      _ -> do mapM_ (swapByteRGBA . (plusPtr image) . (bytespp *)) [0..(size - 1)] -- 4

-- converts from bgr to rgb
swapByteRGB :: Ptr Word8 -> IO ()
swapByteRGB ptr = do
   [b,g,r] <- peekArray 3 ptr
   pokeArray ptr [r,g,b]

-- converts from bgra to rgba
swapByteRGBA :: Ptr Word8 -> IO ()
swapByteRGBA ptr = do
   [b,g,r,a] <- peekArray 4 ptr
   pokeArray ptr [r,g,b,a]

-- returns the pixel format given the bits per pixel
getFormat :: Int ->  IO PixelFormat
getFormat bpp = case bpp of 32 -> return RGBA
                            _  -> return RGB -- 24
