module Game.Loaders.Obj.Processor where

import Data.ByteString.Char8 (ByteString)
import Data.Foldable         (toList)
import Data.Monoid           ((<>))
import Data.Sequence         (empty, index, fromList, (|>))
import Linear                (V3(..))

import Game.Loaders.Obj.Parser (runObjParser)
import Game.Types
  ( ObjData(..)
  , ObjLine(..)
  , VertDataSeq(..) )

parseVertData :: ByteString -> ObjData
parseVertData = pieceTogether . runObjParser

pieceTogether :: [ObjLine] -> ObjData
pieceTogether objLines =
  fromVertData . pieceTogether' objLines $ VertDataSeq empty empty empty
  where
    fromVertData (VertDataSeq v n t) =
        ObjData { _verts     = toList v
                , _norms     = toList n
                , _texCoords = toList t
                , _texIds    = []
                , _texObjs   = []
                , _diffuse   = [] }

pieceTogether' :: [ObjLine] -> VertDataSeq -> VertDataSeq
-- processing vertex
pieceTogether' (LineVert vert : objLines) (VertDataSeq vAccum nAccum tAccum) =
  pieceTogether' objLines (VertDataSeq (vAccum |> vert) nAccum tAccum)
-- processing vertex normal
pieceTogether' (LineNorm norm : objLines) (VertDataSeq vAccum nAccum tAccum) =
  pieceTogether' objLines (VertDataSeq vAccum (nAccum |> norm) tAccum)
-- processing vertex texture coordinate
pieceTogether' (LineTex tex : objLines) (VertDataSeq vAccum nAccum tAccum) =
  pieceTogether' objLines (VertDataSeq vAccum nAccum (tAccum |> tex))
-- processing face data (e.g. f 5/1/6)
pieceTogether' (LineFace (V3 (V3 v1 vt1 vn1)
                             (V3 v2 vt2 vn2)
                             (V3 v3 vt3 vn3)) : objLines)
               (VertDataSeq vAccum nAccum tAccum) =
  let vert1 = vAccum `index` (v1  - 1)
      tex1  = tAccum `index` (vt1 - 1)
      norm1 = nAccum `index` (vn1 - 1)

      vert2 = vAccum `index` (v2  - 1)
      tex2  = tAccum `index` (vt2 - 1)
      norm2 = nAccum `index` (vn2 - 1)

      vert3 = vAccum `index` (v3  - 1)
      tex3  = tAccum `index` (vt3 - 1)
      norm3 = nAccum `index` (vn3 - 1)

      currentVertData = VertDataSeq (fromList [vert1, vert2, vert3])
                                    (fromList [norm1, norm2, norm3])
                                    (fromList [tex1,  tex2,  tex3 ])

      restOfVertData = pieceTogether' objLines $ VertDataSeq vAccum
                                                             nAccum
                                                             tAccum

  in currentVertData <> restOfVertData
-- processing reference to material file
pieceTogether' (MtlRef _ : objLines)  accum = pieceTogether' objLines accum
pieceTogether' (Invalid _ : objLines) accum = pieceTogether' objLines accum
pieceTogether' [] _ = mempty
