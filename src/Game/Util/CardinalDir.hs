module Game.Util.CardinalDir where

import qualified Linear as L

import           Game.Types (CardinalDir(..))

posX :: L.V3 Float
posX = L.V3 1 0 0

posZ :: L.V3 Float
posZ = L.V3 0 0 (-1)

negX :: L.V3 Float
negX = L.V3 (-1) 0 0

negZ :: L.V3 Float
negZ = L.V3 0 0 1

neutral :: L.V3 Float
neutral = L.V3 0 0 0

toDir :: CardinalDir -> L.V3 Float -> CardinalDir
toDir dir vec
  | vec == negX = CardinalDir'West
  | vec == posX = CardinalDir'East
  | vec == posZ = CardinalDir'North
  | vec == negZ = CardinalDir'South
  | otherwise   = dir

fromDir :: CardinalDir -> L.V3 Float
fromDir CardinalDir'West  = negX
fromDir CardinalDir'East  = posX
fromDir CardinalDir'North = posZ
fromDir CardinalDir'South = negZ

toRadAngle :: CardinalDir -> Float
toRadAngle CardinalDir'North = 0
toRadAngle CardinalDir'East  = pi / 2
toRadAngle CardinalDir'South = pi
toRadAngle CardinalDir'West  = 3 * (pi / 2)

toOpposingDir :: CardinalDir -> CardinalDir
toOpposingDir CardinalDir'West  = CardinalDir'East
toOpposingDir CardinalDir'East  = CardinalDir'West
toOpposingDir CardinalDir'North = CardinalDir'South
toOpposingDir CardinalDir'South = CardinalDir'North
