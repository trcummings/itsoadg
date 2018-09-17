module Game.Util.BufferObjects where

import qualified Data.Vector.Storable      as V
import qualified Graphics.Rendering.OpenGL as GL
import           SDL                 (($=))
import           Data.Word           (Word32)
import           Data.Array.Storable (newListArray, withStorableArray)
import           Data.ByteString     (ByteString, useAsCStringLen)
import           Foreign.ForeignPtr  (ForeignPtr, withForeignPtr)
import           Foreign.Ptr         (Ptr, wordPtrToPtr)
import           Foreign.Storable    (Storable, sizeOf)

type BufferInfo = (GL.BufferUsage, GL.BufferTarget)

-- | A class for things we know how to serialize into an OpenGL
-- buffer.
class BufferSource v where
  fromSource :: BufferInfo -> v -> IO GL.BufferObject

instance Storable a => BufferSource [a] where
  fromSource = makeBuffer

instance Storable a => BufferSource (V.Vector a) where
  fromSource = fromVector

-- |Allocate and fill a 'BufferObject' from a list of 'Storable's.
makeBuffer :: Storable a => BufferInfo -> [a] -> IO GL.BufferObject
makeBuffer (usage, target) elems =
  makeBufferLen (usage, target) (length elems) elems

-- |Allocate and fill a 'BufferObject' from a list of 'Storable's
-- whose length is explicitly given. This is useful when the list is
-- of known length, as it avoids a traversal to find the length.
makeBufferLen :: forall a. Storable a
              => BufferInfo
              -> Int
              -> [a]
              -> IO GL.BufferObject
makeBufferLen (usage, target) len elems = do
  [buffer] <- GL.genObjectNames 1
  GL.bindBuffer target $= Just buffer
  let n = fromIntegral $ len * sizeOf (undefined :: a)
  arr <- newListArray (0, len - 1) elems
  withStorableArray arr $ \ptr ->
    GL.bufferData target $= (n, ptr, usage)
  return buffer

-- |@replaceBuffer target elements@ replaces the buffer data attached
-- to the buffer object currently bound to @target@ with the supplied
-- list. Any previous data is deleted.
replaceBuffer :: forall a. Storable a
              => BufferInfo
              -> [a]
              -> IO ()
replaceBuffer (usage, target) elems = do
  arr <- newListArray (0, len - 1) elems
  withStorableArray arr $ \ptr ->
    GL.bufferData target $= (n, ptr, usage)
  where len = length elems
        n = fromIntegral $ len * sizeOf (undefined :: a)

-- |Allocate and fill a 'BufferObject' with the given number of bytes
-- from the supplied pointer.
fromPtr :: BufferInfo
        -> Int
        -> Ptr a
        -> IO GL.BufferObject
fromPtr (usage, target) numBytes ptr =
  do [buffer] <- GL.genObjectNames 1
     GL.bindBuffer target $= Just buffer
     GL.bufferData target $= (fromIntegral numBytes, ptr, usage)
     return buffer

-- |Fill a buffer with a 'ByteString'.
fromByteString :: BufferInfo
               -> ByteString
               -> IO GL.BufferObject
fromByteString (usage, target) b =
  useAsCStringLen b (uncurry . flip $ fromPtr (usage, target))

-- |Fill a buffer with data from a 'ForeignPtr'. The application
-- @fromForeignPtr target len fptr@ fills a @target@ 'BufferTarget'
-- with @len@ elements starting from @fptr@.
fromForeignPtr :: forall a. Storable a
               => BufferInfo
               -> Int
               -> ForeignPtr a
               -> IO GL.BufferObject
fromForeignPtr (usage, target) len fptr =
  withForeignPtr fptr $ fromPtr (usage, target) numBytes
  where numBytes = sizeOf (undefined :: a) * len

-- |Fill a buffer with data from a 'V.Vector'.
fromVector :: forall a. Storable a
           => BufferInfo
           -> V.Vector a
           -> IO GL.BufferObject
fromVector (usage, target) v = V.unsafeWith v $ fromPtr (usage, target) numBytes
  where numBytes = fromIntegral $ V.length v * sizeOf (undefined :: a)

-- |@replaceVector target v@ replaces the buffer data attached to the
-- buffer object currently bound to @target@ with the supplied
-- 'V.Vector'. Any previous data is deleted.
replaceVector :: forall a. Storable a
              => BufferInfo
              -> V.Vector a
              -> IO ()
replaceVector (usage, target) v = do
  V.unsafeWith v $ \ptr ->
    GL.bufferData target $= (numBytes, ptr, usage)
  where numBytes = fromIntegral $ V.length v * sizeOf (undefined :: a)

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

-- | Create an 'ElementArrayBuffer' from a source of 'Word32's.
bufferIndices :: BufferSource (v Word32) => v Word32 -> IO GL.BufferObject
bufferIndices = fromSource (GL.StaticDraw, GL.ElementArrayBuffer)
