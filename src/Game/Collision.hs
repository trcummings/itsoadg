module Game.Collision where

import Linear (V2(..))
import Apecs (Entity)

import Game.Types (Unit(..))

data CNormal =
    LeftN
  | RightN
  | TopN
  | BottomN
  | NoneN -- the "zero normal"
  deriving (Eq, Show)

data Collision =
  Collision Double CNormal Entity
  deriving Show

toVector :: CNormal -> V2 Unit
toVector LeftN   = V2 (-1)  0
toVector RightN  = V2   1   0
toVector TopN    = V2   0   1
toVector BottomN = V2   0 (-1)
toVector NoneN   = V2   0   0
