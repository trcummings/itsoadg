module Game.Util.Terrain (generateTerrain, TerrainInfo(..), size, intVertexCount) where

import qualified Linear as L

data TerrainInfo = TerrainInfo
  { _trVertices  :: [L.V3 Float]
  , _trTexCoords :: [L.V2 Float]
  , _trNormals   :: [L.V3 Float]
  , _trIndices   :: [L.V3 Int] }
  deriving Show

-- instance Monoid TerrainInfo where
--   mempty = TerrainInfo [] [] [] []
--   mappend (TerrainInfo v1 t1 n1 i1)
--           (TerrainInfo v2 t2 n2 i2) =
--     TerrainInfo (v1 ++ v2) (t1 ++ t2) (n1 ++ n2) (i1 ++ i2)

size :: Float
size = 800

flVertexCount :: Float
flVertexCount = 128

intVertexCount :: Int
intVertexCount = 128

flCountList :: [Float]
flCountList = [0..(flVertexCount - 1)]

intCountList :: [Int]
intCountList = [0..(intVertexCount - 1)]

generateTerrain :: TerrainInfo
generateTerrain =
  TerrainInfo { _trVertices  = [ L.V3 ((j / ((flVertexCount - 1)) * size))
                                      0
                                      ((i / ((flVertexCount - 1)) * size))
                               | i <- flCountList
                               , j <- flCountList ]

              , _trTexCoords = [ L.V2 (j / (flVertexCount - 1))
                                      (i / (flVertexCount - 1))
                               | i <- flCountList
                               , j <- flCountList ]

              , _trNormals   = [ L.V3 0 1 0
                               | _ <- flCountList
                               , _ <- flCountList ]

              , _trIndices   = concat [ [ L.V3 topLeft  botLeft topRight
                                        , L.V3 topRight botLeft botRight ]
                                      | gz <- intCountList
                                      , gx <- intCountList
                                      , let topLeft  = (gz * intVertexCount) + gx
                                            topRight = topLeft + 1
                                            botLeft  = ((gz + 1) * intVertexCount) + gx
                                            botRight = botLeft + 1
                                      ] }
