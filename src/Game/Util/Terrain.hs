module Game.Util.Terrain (generateTerrain, TerrainInfo(..), vertexCount) where

import qualified Linear as L

data TerrainInfo = TerrainInfo
  { _trVertices  :: [L.V3 Float]
  , _trTexCoords :: [L.V2 Float]
  , _trNormals   :: [L.V3 Float]
  , _trIndices   :: [L.V3 Float] }
  deriving Show

-- instance Monoid TerrainInfo where
--   mempty = TerrainInfo [] [] [] []
--   mappend (TerrainInfo v1 t1 n1 i1)
--           (TerrainInfo v2 t2 n2 i2) =
--     TerrainInfo (v1 ++ v2) (t1 ++ t2) (n1 ++ n2) (i1 ++ i2)

size :: Float
size = 800

vertexCount :: Float
vertexCount = 128

countList :: [Float]
countList = [0..(vertexCount - 1)]

toVert :: Float -> Float
toVert x = (x / (vertexCount - 1)) * size

generateTerrain :: TerrainInfo
generateTerrain =
  TerrainInfo { _trVertices  = [ L.V3 (toVert j)
                                      0
                                      (toVert i)
                               | i <- countList
                               , j <- countList ]

              , _trTexCoords = [ L.V2 (j / (vertexCount - 1))
                                      (i / (vertexCount - 1))
                               | i <- countList
                               , j <- countList ]

              , _trNormals   = [ L.V3 0 1 0 | _ <- countList, _ <- countList ]

              , _trIndices   = concat [ [ L.V3 topLeft  botLeft topRight
                                        , L.V3 topRight botLeft botRight ]
                                      | gz <- countList
                                      , gx <- countList
                                      , let topLeft  = (gz * vertexCount) + gx
                                            topRight = topLeft + 1
                                            botLeft  = ((gz + 1) * vertexCount) + gx
                                            botRight = botLeft + 1
                                      ] }
