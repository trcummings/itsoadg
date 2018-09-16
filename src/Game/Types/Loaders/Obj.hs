module Game.Types.Loaders.Obj
  ( ObjData(..)
  , VertDataSeq(..)
  , ObjLine(..) ) where

import Graphics.Rendering.OpenGL (GLfloat, TextureObject, GLint)
import Data.ByteString.Char8     (ByteString)
import Data.Sequence             (empty, Seq, (|>), (><))
import Data.Monoid               (Monoid(..), (<>))
import Linear                    (V2, V3)

data ObjData = ObjData
  { _verts     :: [V3 GLfloat]    -- Vertices
  , _norms     :: [V3 GLfloat]    -- Normals
  , _texCoords :: [V2 GLfloat]    -- U/V Texture coords
  , _texIds    :: [GLint]         -- Texture ID
  , _texObjs   :: [TextureObject] -- Texture Objects
  , _diffuse   :: [V3 GLfloat]  } -- Diffuse
  deriving Show

data VertDataSeq = VertDataSeq
  (Seq (V3 GLfloat))
  (Seq (V3 GLfloat))
  (Seq (V2 GLfloat))

instance Monoid VertDataSeq where
  mempty  = VertDataSeq empty empty empty
  mappend (VertDataSeq v n t) (VertDataSeq v2 n2 t2) =
    VertDataSeq (v >< v2) (n >< n2) (t >< t2)

data ObjLine =
    LineVert (V3 GLfloat)
  | LineNorm (V3 GLfloat)
  | LineTex  (V2 GLfloat)
  | LineFace (V3 (V3 Int))
  | MtlRef   FilePath
  | Invalid  ByteString
    deriving (Show, Eq)
