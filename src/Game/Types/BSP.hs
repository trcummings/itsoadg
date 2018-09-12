{-# LANGUAGE BangPatterns #-}

module Game.Types.BSP where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Array.IO             as IOArr
import           Linear          (V3, V2)
import           Foreign.C.Types (CFloat)
import           Data.Word       (Word8)
import           Foreign.Ptr     (Ptr)

type VertexArrays =
  ( Ptr Float
  , Ptr Float
  , Ptr Float
  , Ptr Float
  , Ptr Word8 )

type VertexArrayData =
  ( [CFloat]
  , [CFloat]
  , [CFloat]
  , [CFloat]
  , [Word8] )

type VertexData =
  ( [Float]
  , [Float]
  , [Float]
  , [Float]
  , [Word8] )

type VertexPointers =
  ( Ptr GL.GLfloat
  , Ptr GL.GLfloat
  , Ptr GL.GLfloat
  , Ptr GL.GLint )

type BSPLeafFace = Int

newtype BitSet = BitSet (IOArr.IOUArray Int Bool)

data Tree =
    Leaf   BSPLeaf
  | Branch BSPNode Tree Tree

data BSPMap = BSPMap
  { _vertexData :: !VertexArrays
  , _vindices   :: !(Ptr GL.GLint)
  , _leaves     :: ![BSPLeaf]
  , _tree       :: !Tree
  , _visData    :: !(Maybe BSPVisData)
  , _bitset     :: !BitSet
  , _buffers    :: BSPBuffers }

data BSPLeaf = BSPLeaf
  { _cluster          :: !Int
  , _area             :: Int
  , _leafMin          :: V3 Double
  , _leafMax          :: V3 Double
  , _leafface         :: Int
  , _numOfLeafFaces   :: Int
  , _leafBrush        :: Int
  , _numOfLeafBrushes :: Int
  , _leafFaces        :: [BSPFace]
  , _leafBrushes      :: [BSPBrush] }
  deriving Show

data BSPFace = BSPFace
    -- The index into the texture array
  { _textureObj     :: Maybe GL.TextureObject
    -- The index for the effects (or -1 = n/a)
  , _effect         :: Int
    -- 1 = polygon, 2 = patch, 3 = mesh, 4 = billboard
  , _faceType       :: Int
    -- The starting index into this face's first vertex
  , _startVertIndex :: Int
    -- The number of vertices for this face
  , _numOfVerts     :: Int
    -- The starting index into the indices array for this face
  , _startIndex     :: Int
    -- The number of indices for this face
  , _numOfIndices   :: GL.GLint
    -- The texture index for the lightmap
  , _lightmapObj    :: Maybe GL.TextureObject
    -- The face's lightmap corner in the image
  , _lMapCorner     :: V2 Int
    -- The size of the lightmap section
  , _lMapSize       :: V2 Int
    -- The 3D origin of lightmap
  , _lMapPos        :: V3 Float
    -- The 3D space for s and t unit vectors
  , _lMapVecs       :: [V3 Float]
    -- The face normal.
  , _vNormal        :: V3 Float
    -- The bezier patch dimensions
  , _size           :: V2 Int
  , _faceNo         :: Int
  , _patch          :: [BSPPatch]
  , _arrayPtrs      :: VertexPointers }
  deriving Show

data BSPBrush = BSPBrush
  { _brushSide       :: Int
  , _numOfBrushSides :: Int
  , _brushSides      :: [BSPBrushSide]
  , _bTextureID      :: Int
  , _textureType     :: Int }
  deriving Show

data BSPBrushSide = BSPBrushSide
  { _bsPlane     :: Int
  , _bsPlaneNorm :: V3 Double
  , _bsPlaneDist :: Double
  , _bsTextureID :: Int }
  deriving Show

data BSPNode = BSPNode
  { _planeNormal :: V3 Double
  , _dist        :: Double
  , _front       :: Int
  , _back        :: Int
  , _nodeMin     :: V3 Int
  , _nodeMax     :: V3 Int }
  deriving Show

data BSPVisData = BSPVisData
  { _numOfClusters   :: Int
  , _bytesPerCluster :: Int
  , _bitSets         :: IOArr.IOUArray Int Bool }

data BSPLump = BSPLump
  { _offset :: Int
  , _len    :: Int }
  deriving Show

data BSPHeader = BSPHeader
  { _strID   :: String
  , _version :: Int }
  deriving Show

data BSPTexInfo = BSPTexInfo
  { _strName  :: String
  , _flags    :: Int
  , _contents :: Int }
  deriving Show

data BSPPlane = BSPPlane
  { _pNormal  :: V3 Double
  , _distance :: Double }
  deriving Show

data BSPPatch = BSPPatch
  -- the level of tesselation
  { _patchLOD    :: Int
  -- points to patch vertices
  , _patchPtr    :: Ptr Float
  -- points to indices
  , _indexPtrPtr :: Ptr (Ptr GL.GLint)
  -- the number of indices
  , _numIndexPtr :: Ptr GL.GLsizei }
  deriving Show

data BSPBuffers = BSPBuffers
  { _bspPosition  :: GL.BufferObject
  , _bspTexCoords :: GL.BufferObject
  , _bspLmpCoords :: GL.BufferObject }
