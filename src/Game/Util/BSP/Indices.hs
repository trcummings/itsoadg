module Game.Util.BSP.Indices where
-- This file is for lump directory indices

-- Stores game-related object descriptions. CURRENTLY UNUSED
-- kEntities :: Int
-- kEntities = 0

-- Stores texture information
kTextures :: Int
kTextures = 1

-- Stores the splitting planes
kPlanes :: Int
kPlanes = 2

-- Stores the BSP nodes
kNodes :: Int
kNodes = 3

-- Stores the leafs of the nodes
kLeafs :: Int
kLeafs = 4

-- Stores the leaf's indices into the faces
kLeafFaces :: Int
kLeafFaces = 5

-- Stores the leaf's indices into the brushes
kLeafBrushes :: Int
kLeafBrushes = 6

-- Stores rigid world geometry data. CURRENTLY UNUSED
-- kModels :: Int
-- kModels = 7

-- Stores the brushes info (for collision)
kBrushes :: Int
kBrushes = 8

-- Stores the brush surfaces
kBrushSides :: Int
kBrushSides = 9

-- Stores the level vertices
kVertices :: Int
kVertices = 10

-- Stores the level indices (mesh vertexes)
kIndices :: Int
kIndices = 11

-- Stores special shader effects on the map. CURRENTLY UNUSED
-- kEffects :: Int
-- kEffects = 12

-- Stores the faces for the level
kFaces :: Int
kFaces = 13

-- Stores the lightmaps for the level
kLightmaps :: Int
kLightmaps = 14

-- Stores local illumination data. CURRENTLY UNUSED
-- kLightvols :: Int
-- kLightvols = 15

-- Stores PVS and cluster info (visibility)
kVisData :: Int
kVisData = 16

-- A constant to store the number of lumps
kMaxLumps :: Int
kMaxLumps = 17
