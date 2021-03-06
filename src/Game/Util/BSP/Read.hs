module Game.Util.BSP.Read where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Array.MArray         as Arr (readArray, newListArray)
import qualified Linear                    as L
import           Foreign.Marshal
  ( mallocBytes
  , advancePtr
  , pokeArray
  , peekArray
  , newArray
  , free )
import           Foreign.Storable (peek, peekElemOff, peekByteOff)
import           Foreign.C.String (peekCAString, castCCharToChar)
import           Foreign.Ptr      (Ptr, castPtr, plusPtr)
import           Foreign.C.Types  (CInt, CChar, CFloat)
import           Data.Array       (Array, (!), listArray)
import           Data.Word        (Word8)
import           Data.List        (unzip5)
import           Control.Monad    ((<=<))
import           SDL              (($=))
import           System.FilePath  ((</>))
import           System.IO
  ( Handle
  , SeekMode(AbsoluteSeek)
  , hGetBuf
  , hSeek )

import           Game.Types              (Texture(..))
import           Game.Loaders.Texture    (getAndCreateTextures)
import           Game.Loaders.File       (withBinaryFile)
import           Game.Util.GLError       (printGLErrors)
import           Game.Util.Constants     (assetPath)
import           Game.Util.BufferObjects (fromSource)
import           Game.Util.BSP.Curves    (checkForPatch)
import           Game.Util.BSP.Indices
import           Game.Util.BSP.BitSet
import           Game.Util.BSP.Util
import           Game.Types.BSP

-- reads a BSP file
readBSP :: FilePath -> IO BSPMap
readBSP filePath = withBinaryFile filePath $ \handle -> do
  printGLErrors "readBSP pre-read"
  -- read header from .bsp file. we don't need it at the moment, but we
  -- need to move the file handle along to the lumps
  readHeader handle
  -- read lumps from the .bsp file. the lumps will give us a list of
  -- lumps, which give us memory length & offset for where each lump is
  -- in memory. we use the kIndices from Game.Util.BSP.Indices to index
  -- into the desired lump to read its data
  lumps           <- mapM (readLump handle) [0..(kMaxLumps - 1)] :: IO [BSPLump]
  (a, b, c, d, e) <- readVertices handle lumps
  -- why are the first 48 indices garbage? (0 or 1098907648)
  indices         <- readIndices  handle lumps
  newbitset       <- createBitset lumps
  -- create memory pointers for our 5 vertex arrays
  newVertexArrays <- dataToPointers (a, b, c, d, e)
  indexPtr        <- newArray indices
  -- putStrLn $ show a
  -- putStrLn $ show indices
  -- putStrLn $ show indexPtr
  newNodes        <- readNodes   handle lumps
  newLeaves       <- readLeaves  handle lumps newVertexArrays indexPtr
  newVisData      <- readVisData handle lumps
  let leafArray = listArray (0, length newLeaves - 1) newLeaves
      nodeArray = listArray (0, length newNodes  - 1) newNodes
  -- create the node tree to traverse later
  ntree   <- constructTree nodeArray leafArray 0
  -- make buffers
  printGLErrors "readBSP pre-create buffers"
  buffers <- BSPBuffers <$> fromSource (GL.StaticDraw, GL.ArrayBuffer) a
                        <*> fromSource (GL.StaticDraw, GL.ArrayBuffer) b
                        <*> fromSource (GL.StaticDraw, GL.ArrayBuffer) c
                        <*> fromSource (GL.StaticDraw, GL.ArrayBuffer) d
                        <*> fromSource (GL.StaticDraw, GL.ElementArrayBuffer) indices
  printGLErrors "readBSP post-create buffers"
  -- return BSP map
  return BSPMap { _vertexData = newVertexArrays
                , _vIndices   = indexPtr
                , _leaves     = reverse newLeaves
                , _tree       = ntree
                , _visData    = newVisData
                , _bitset     = newbitset
                , _buffers    = buffers }


createBitset :: [BSPLump] -> IO BitSet
createBitset lumps = do
  (_, lngth) <- getLumpData (lumps !! kFaces)
  emptyBS (lngth `div` 104) -- why 104?



-- reads the BSP files header information
readHeader :: Handle -> IO BSPHeader
readHeader handle = do
  buf <- mallocBytes 4
  hGetBuf handle buf 4
  iD  <- mapM (peekByteOff buf) [0..3] :: IO [CChar]
  hGetBuf handle buf cIntSize
  ver <- peek (castPtr buf :: Ptr CInt) :: IO CInt
  free buf
  return BSPHeader { _strID   = map castCCharToChar iD
                   , _version = fromIntegral ver }


-- reads the lumps in our bsp
readLump :: Handle -> Int -> IO BSPLump
readLump handle _ = do
  -- allocate a number of bytes equal to the size of a CInt
  buf     <- mallocBytes cIntSize
  hGetBuf handle buf cIntSize
  offset  <- peek (castPtr buf :: Ptr CInt) :: IO CInt
  hGetBuf handle buf cIntSize
  length' <- peek (castPtr buf :: Ptr CInt) :: IO CInt
  free buf
  return BSPLump { _offset = fromIntegral offset
                 , _len    = fromIntegral length' }

getLumpData :: BSPLump -> IO (Int, Int)
getLumpData lump = return (_offset lump, _len lump)


-- reads the nodes
readNodes :: Handle -> [BSPLump] -> IO [BSPNode]
readNodes handle lumps = do
  planes <- readPlanes handle lumps
  let planeArray = listArray (0, length planes - 1) planes
  (offst, lngth) <- getLumpData (lumps !! kNodes)
  offs           <- getOffsets lngth offst 36
  mapM (readNode handle planeArray) offs

readNode :: Handle -> Array Int BSPPlane -> Int -> IO BSPNode
readNode handle planeArray offst = do
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes 4
  let getCInt  = getAndPeek  handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
      getCInts = getAndPeeks handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
      ints     = fmap toInts (getCInts 3)
      get3Ints = fmap get3t ints
  plnIndex              <- getCInt
  frt                   <- getCInt
  bck                   <- getCInt
  (nMin1, nMin2, nMin3) <- get3Ints
  (nMax1, nMax2, nMax3) <- get3Ints
  let pln = planeArray ! fromIntegral plnIndex
  return BSPNode { _planeNormal = _pNormal pln
                 , _dist        = _distance pln
                 , _front       = fromIntegral frt
                 , _back        = fromIntegral bck
                 , _nodeMin     = L.V3 nMin1 nMin2 nMin3
                 , _nodeMax     = L.V3 nMax1 nMax2 nMax3 }


-- reads the planes in the nodes

-- plane normal - float[3]
-- distance     - float    (from origin to plane alone normal)
-- NB: 4 floats = size of 16 bytes
readPlanes :: Handle -> [BSPLump] -> IO [BSPPlane]
readPlanes handle lumps = do
  (offst, lngth) <- getLumpData (lumps !! kPlanes)
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes lngth
  hGetBuf handle buf lngth
  let ptrs = getPtrs buf lngth 16
  planes <- mapM readPlane ptrs
  free buf
  return planes

readPlane :: Ptr a -> IO BSPPlane
readPlane ptr = do
  [e1, e2, e3, e4] <- getFloats ptr 4
  return BSPPlane { _pNormal  = L.V3
                                  (fromRational (toRational e1))
                                  (fromRational (toRational e3))
                                  (fromRational (toRational $ (-1) * e2))
                  , _distance = fromRational (toRational e4) }


-- reads the leaves
readLeaves :: Handle -> [BSPLump] -> VertexArrays -> Ptr GL.GLint -> IO [BSPLeaf]
readLeaves handle lumps vertArrays indcs = do
  faces              <- readFaces handle lumps vertArrays indcs
  let faceArray      =  listArray (0, length faces - 1)  faces
  leafFaces          <- readLeafFaces handle lumps
  let leafFaceArray  =  listArray (0, length leafFaces - 1) leafFaces
  brushes            <- readBrushes handle lumps
  let brushArray     =  listArray (0, length brushes - 1) brushes
  leafbrushes        <- readLeafBrushes handle lumps
  let leafBrushArray =  listArray (0, length leafbrushes - 1) leafbrushes
  (offst, lngth)     <- getLumpData (lumps !! kLeafs)
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes lngth
  hGetBuf handle buf lngth
  let ptrs = getPtrs buf lngth 48
  nodes <- mapM (readLeaf leafFaceArray faceArray leafBrushArray brushArray) ptrs
  free buf
  return nodes


readLeaf :: Array Int Int
         -> Array Int BSPFace
         -> Array Int Int
         -> Array Int BSPBrush
         -> Ptr a
         -> IO BSPLeaf
readLeaf leafFaceArray faceArray leafBrushArray brushArray ptr = do
  [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] <- getInts ptr 12
  let leafIndices  = map (leafFaceArray !) [ ((e9 + e10) - 1)
                                           , ((e9 + e10) - 2)..e9 ]
      brushIndices = map (leafBrushArray !) [e11..(e11 + e12 - 1)]
      faceList     = map (faceArray !) leafIndices
      brushList    = map (brushArray !) brushIndices
  return BSPLeaf { _cluster          = e1
                 , _area             = e2
                 -- xzy -> xyz
                 , _leafMin          = L.V3
                                         (realToFrac e3)
                                         (realToFrac e5)
                                         (realToFrac $ (-1) * e4)
                 , _leafMax          = L.V3
                                         (realToFrac e6)
                                         (realToFrac e8)
                                         (realToFrac $ (-1) * e7)
                 , _leafface         = e9
                 , _numOfLeafFaces   = e10
                 , _leafBrush        = e11
                 , _numOfLeafBrushes = e12
                 , _leafFaces        = faceList
                 , _leafBrushes      = brushList }



-- huge functions for reading the faces in our leaves
readFaces :: Handle -> [BSPLump] -> VertexArrays -> Ptr GL.GLint -> IO [BSPFace]
readFaces handle lumps vertArrays indcs = do
  lightMaps         <- readLightMaps handle lumps
  let lightMapArray =  listArray (0, length lightMaps - 1) lightMaps
  texInfos          <- readTexInfos handle lumps
  let texFileNames  =  map (\p -> assetPath </> (_strName p) ++ ".tga") texInfos
  texObjs           <- map _textureId <$> getAndCreateTextures texFileNames
  let texObjArray   =  listArray (0, length texObjs - 1) texObjs
  (offst, lngth)    <- getLumpData (lumps !! kFaces)
  offs              <- getOffsets lngth offst 104
  mapM (readFace handle offst lightMapArray texObjArray vertArrays indcs) offs


readFace :: Handle
         -> Int
         -> Array Int GL.TextureObject
         -> Array Int (Maybe GL.TextureObject)
         -> VertexArrays
         -> Ptr GL.GLint
         -> Int
         -> IO BSPFace
readFace
  handle
  origin
  lightmaps
  textures
  vertArrays@(a1, b1, c1, _, _)
  indices
  offset = do
    hSeek handle AbsoluteSeek (fromIntegral offset)
    buf <- mallocBytes 4
    let getCInts   = getAndPeeks handle (castPtr buf :: Ptr CInt)   (undefined :: CInt)
        getCFloats = getAndPeeks handle (castPtr buf :: Ptr CFloat) (undefined :: CFloat)
        ints       = toInts   <$> (getCInts   4)
        get4Ints   = get4t    <$> ints
        floats     = toFloats <$> (getCFloats 3)
        get3Floats = get3t    <$> floats
        twoInts    = toInts   <$> (getCInts   2)
        get2Ints   = get2t    <$> twoInts
    -- texture index, effect, face type, idx of first vertex
    (a, b, c, d)          <- get4Ints
    -- num vertices, idx of first meshvert, num meshverts, lightmap index
    (e, f, g, h)          <- get4Ints
    -- int[2] corner of face's lightmap, int[2] size of face's lightmap image
    (i, j, k, l)          <- get4Ints
    -- world space origin of lightmap
    (lMPs1, lMPs2, lMPs3) <- get3Floats
    -- world space lightmap s & t unit vectors
    (lMV11, lMV12, lMV13) <- get3Floats
    (lMV21, lMV22, lMV23) <- get3Floats
    -- surface normals
    (n1, n2, n3)          <- get3Floats
    -- patch dimensions
    (size1, size2)        <- get2Ints
    free buf
    bspPatch <- checkForPatch c d (size1, size2) vertArrays
    return BSPFace { _textureObj     = textures ! a
                   , _effect         = b
                   , _faceType       = c
                   , _startVertIndex = d
                   , _numOfVerts     = fromIntegral e
                   , _startIndex     = f
                   , _numOfIndices   = fromIntegral g
                   , _lightmapObj    = fixLightmap h lightmaps
                   , _lMapCorner     = L.V2 i j
                   , _lMapSize       = L.V2 k l
                   , _lMapPos        = L.V3 lMPs1 lMPs2 lMPs3
                   , _lMapVecs       = [ L.V3 lMV11 lMV12 lMV13
                                       , L.V3 lMV21 lMV22 lMV23 ]
                   , _vNormal        = L.V3 n1 n2 n3
                   , _size           = L.V2 size1 size2
                   , _faceNo         = (offset - origin) `div` 104
                   , _patch          = bspPatch
                   -- create pointer references to the face's position
                   -- in the position, texCoord, lightmap arrays, &
                   -- where it is in the indices
                   , _arrayPtrs      = ( plusPtr a1      (12 * d)
                                       , plusPtr b1      (8  * d)
                                       , plusPtr c1      (8  * d)
                                       , plusPtr indices (4  * f) ) }


-- reads the leaf faces that refer to the faces
readLeafFaces :: Handle -> [BSPLump] -> IO [BSPLeafFace]
readLeafFaces handle lumps = do
  (offst, lngth) <- getLumpData (lumps !! kLeafFaces)
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes lngth
  hGetBuf handle buf lngth
  leafFaces <- getInts buf (lngth `div` 4)
  free buf
  return leafFaces


-- reads the brushes
readBrushes :: Handle -> [BSPLump] -> IO [BSPBrush]
readBrushes handle lumps = do
  brushsides         <- readBrushSides handle lumps
  let brushSideArray =  listArray (0, length brushsides - 1) brushsides
  texInfos           <- readTexInfos handle lumps
  let texInfoArray   =  listArray (0, length texInfos   - 1) texInfos
  (offst, lngth)     <- getLumpData (lumps !! kBrushes)
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf                <- mallocBytes lngth
  hGetBuf handle buf lngth
  let ptrs           = getPtrs buf lngth 12
  brushes            <- mapM (readBrush brushSideArray texInfoArray) ptrs
  free buf
  return brushes

readBrush :: Array Int BSPBrushSide
          -> Array Int BSPTexInfo
          -> Ptr a
          -> IO BSPBrush
readBrush brushSideArray texInfos ptr = do
  [e1, e2, e3] <- getInts ptr 3
  let bSides = map (brushSideArray !) [e1..(e1 + e2 - 1)]
  return BSPBrush { _brushSide        = e1
                  , _numOfBrushSides  = e2
                  , _brushSides       = bSides
                  , _bTextureID       = e3
                  , _textureType      = _contents (texInfos ! e3) }



-- reads the brush sides in our brushes
readBrushSides :: Handle -> [BSPLump] -> IO [BSPBrushSide]
readBrushSides handle lumps = do
  planes          <- readPlanes handle lumps
  let planeArray  =  listArray (0, length planes - 1) planes
  (offst, lngth)  <- getLumpData (lumps !! kBrushSides)
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf             <- mallocBytes lngth
  hGetBuf handle buf lngth
  let ptrs        =  getPtrs buf lngth 8
  brushsides      <- mapM (readBrushSide planeArray) ptrs
  free buf
  return brushsides

readBrushSide :: Array Int BSPPlane -> Ptr a -> IO BSPBrushSide
readBrushSide planeArray ptr = do
  [e1, e2] <- getInts ptr 2
  let pln = planeArray ! fromIntegral e1
  return BSPBrushSide { _bsPlane     = e1
                      , _bsPlaneNorm = _pNormal pln
                      , _bsPlaneDist = _distance pln
                      , _bsTextureID = e2 }


-- reads the leaf brushes that refer to the brushes
readLeafBrushes :: Handle -> [BSPLump] -> IO [BSPLeafFace]
readLeafBrushes handle lumps = do
  (offst, lngth) <- getLumpData (lumps !! kLeafBrushes)
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes lngth
  hGetBuf handle buf lngth
  leafbrushes <- getInts buf (lngth `div` 4)
  free buf
  return leafbrushes



-- read the PVS visibility information
readVisData :: Handle -> [BSPLump] -> IO (Maybe BSPVisData)
readVisData handle lumps = do
  (offst, lngth) <- getLumpData (lumps !! kVisData)
  case lngth of
   0  -> return Nothing
   _  -> do
      hSeek handle AbsoluteSeek (fromIntegral offst)
      buf <- mallocBytes lngth
      hGetBuf handle buf lngth
      cInts <- peekArray 2 (castPtr buf :: Ptr CInt)
      let [numC, bytesPerC] = toInts cInts
      bitst <- peekArray (numC * bytesPerC) $ plusPtr (castPtr buf :: Ptr Word8) 8
      bs    <- Arr.newListArray (0, numC * bytesPerC * 8 - 1) (toBools bitst)
      return $ Just $ BSPVisData { _numOfClusters   = numC
                                 , _bytesPerCluster = bytesPerC
                                 , _bitSets         = bs }


-- reads vertex information
-- vertex position - float[3]
-- texture coords  - float[2][2] (surface & lightmap)
-- vertex normals  - float[3]
-- color           - ubyte[4]
-- NB: float in C is 4 bytes, ubyte is 1 byte, thus offset of size 44
readVertices :: Handle -> [BSPLump] -> IO VertexData
readVertices handle lumps = do
  (offset, length') <- getLumpData (lumps !! kVertices)
  offsets           <- getOffsets length' offset 44
  verts             <- mapM (readVertex handle) offsets
  (v, t, l, n, r)   <- seperateArrays verts
  return $ toVertexData ( concat v
                        , concat t
                        , concat l
                        , concat n
                        , concat r )


readVertex :: Handle -> Int -> IO VertexArrayData
readVertex handle offst = do
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes 4
  let getCFloats = getAndPeeks handle (castPtr buf :: Ptr CFloat) (undefined :: CFloat)
      getWord8s  = getAndPeeks handle (castPtr buf :: Ptr Word8)  (undefined :: Word8)
      floats     = getCFloats 3
      get3Floats = fmap get3t floats
  (x, y, z)      <- get3Floats
  texCoords      <- getCFloats 2
  lightMapCoords <- getCFloats 2
  normals        <- getCFloats 3
  rgbaVal        <- getWord8s  4
  free buf
  return ( [x, z, (-1) * y]
         , texCoords
         , lightMapCoords
         , normals
         , rgbaVal )

dataToPointers :: VertexData -> IO VertexArrays
dataToPointers (a, b, c, d, e) = do
  a1 <- newArray a
  b1 <- newArray b
  c1 <- newArray c
  d1 <- newArray d
  e1 <- newArray e
  return (a1, b1, c1, d1, e1)

seperateArrays :: [VertexArrayData]
               -> IO ( [[CFloat]]
                     , [[CFloat]]
                     , [[CFloat]]
                     , [[CFloat]]
                     , [[Word8]] )
seperateArrays verts = return (unzip5 verts)

toVertexData :: VertexArrayData -> VertexData
toVertexData (a, b, c, d, e) =
  ( toFloats a
  , toFloats b
  , toFloats c
  , toFloats d
  , e)


-- reads the indices to the vertex array
readIndices :: Handle -> [BSPLump] -> IO [GL.GLint]
readIndices handle lumps = do
  (offset, length') <- getLumpData (lumps !! kIndices)
  hSeek handle AbsoluteSeek (fromIntegral offset)
  buf    <- mallocBytes length'
  hGetBuf handle buf length'
  indices <- mapM
              (peekElemOff (castPtr buf :: Ptr CInt))
              [0..((length' `div` 4) - 1)] :: IO [CInt]
  free buf
  return $ map fromIntegral indices


-- reads lightmaps
-- NB: a lightmap is made of ubyte[128][128][3] map
-- So the size of the entire lightmap lump is 49152, or 128 * 128 * 3
readLightMaps :: Handle -> [BSPLump] -> IO [GL.TextureObject]
readLightMaps handle lumps = do
  (offst, lngth) <- getLumpData (lumps !! kLightmaps)
  offs           <- getOffsets lngth offst 49152
  mapM (readLightMap handle) offs

readLightMap :: Handle -> Int -> IO GL.TextureObject
readLightMap handle offst = do
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes 49152 :: IO (Ptr Word8)
  hGetBuf handle buf 49152
  mapM_ (adjustRGB buf 5.0) [0..(16384 - 1)] -- 128 * 128
  createLightmapTexture buf

createLightmapTexture :: Ptr Word8 -> IO GL.TextureObject
createLightmapTexture ptr = do
  printGLErrors "createLightmapTexture pre-create"
  [texName] <- GL.genObjectNames 1
  GL.rowAlignment   GL.Unpack    $= 1
  GL.textureBinding GL.Texture2D $= Just texName
  GL.build2DMipmaps
    GL.Texture2D GL.RGB'
      (fromIntegral (128 :: Int))
      (fromIntegral (128 :: Int))
      (GL.PixelData GL.RGB GL.UnsignedByte ptr)
  GL.textureFilter GL.Texture2D $= ( (GL.Linear', Just GL.Nearest)
                                   , GL.Linear' )
  GL.textureFunction $= GL.Modulate
  free ptr
  printGLErrors "createLightmapTexture done"
  return texName


-- adjusts the brightness of the lightmap
adjustRGB :: Ptr Word8 -> Float -> Int -> IO ()
adjustRGB lightMap factor offst = do
  let ptr = advancePtr lightMap (3 * offst)
  [r, g, b] <- peekArray 3 ptr
  let (r2, tempr) = scaleRGB ((realToFrac r * factor) / 255) 1
      (g2, tempg) = scaleRGB ((realToFrac g * factor) / 255) tempr
      (b2, tempb) = scaleRGB ((realToFrac b * factor) / 255) tempg
      byter2      = fromIntegral (truncate (r2 * tempb * 255.0) :: Int)
      byteg2      = fromIntegral (truncate (g2 * tempb * 255.0) :: Int)
      byteb2      = fromIntegral (truncate (b2 * tempb * 255.0) :: Int)
  pokeArray (advancePtr lightMap (3 * offst)) [byter2, byteg2, byteb2]
  where
    scaleRGB :: Float -> Float -> (Float,Float)
    scaleRGB clr scl =
      if   (clr > 1.0) && ((1.0 / clr) < scl)
      then (clr, 1.0 / clr)
      else (clr, scl)

fixLightmap :: Int -> Array Int GL.TextureObject -> Maybe GL.TextureObject
fixLightmap ind arr
  | ind < 0   = Nothing
  | otherwise = Just (arr ! ind)


-- reads the texture information
readTexInfos :: Handle -> [BSPLump] -> IO [BSPTexInfo]
readTexInfos handle lumps = do
  (offset, length') <- getLumpData (lumps !! kTextures)
  offsets           <- getOffsets length' offset 72
  mapM (readTexInfo handle) offsets

readTexInfo :: Handle -> Int -> IO BSPTexInfo
readTexInfo handle offst = do
  hSeek handle AbsoluteSeek (fromIntegral offst)
  buf <- mallocBytes 64 :: IO (Ptr CChar)
  hGetBuf handle buf 64
  str <- peekCAString buf
  hSeek handle AbsoluteSeek (fromIntegral offst + 64)
  let getCInt = getAndPeek handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
  flgs <- getCInt
  cons <- getCInt
  free buf
  return BSPTexInfo { _strName  = str
                    , _flags    = fromIntegral flgs
                    , _contents = fromIntegral cons }


constructTree :: Array Int BSPNode -> Array Int BSPLeaf -> Int -> IO Tree
constructTree nodes leaves index =
  if index >= 0
  then do
    let currentNode = nodes ! index
    leftNode  <- constructTree nodes leaves (_front currentNode)
    rightNode <- constructTree nodes leaves (_back  currentNode)
    return $ Branch currentNode leftNode rightNode
  else do
    let currentLeaf = leaves ! ((-1) * (index + 1))
    return $ Leaf currentLeaf
