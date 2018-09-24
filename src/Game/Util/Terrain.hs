module Game.Util.Terrain where

import qualified Linear as L

import Game.Types (TerrainConfig(..), TerrainInfo(..))

generateTerrain :: TerrainConfig -> TerrainInfo
generateTerrain config =
  let size                      = _trSize      config
      flVertexCount  :: Float   = _trVertCount config
      intVertexCount :: Int     = floor flVertexCount
      flCountList    :: [Float] = [0..(flVertexCount -  1)]
      intCountList   :: [Int]   = [0..(intVertexCount - 1)]
  in TerrainInfo
    { _trVertices  = [ L.V3 ((j / ((flVertexCount - 1)) * size))
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
