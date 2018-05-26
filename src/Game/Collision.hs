module Game.Collision where

import Linear (V2(..))
import Apecs (Entity)

import Game.Types (Velocity(..), Position(..), BoundingBox(..))
import Game.Constants (Unit(..), dTinSeconds)

data AABB = AABB
  { center :: (V2 Unit)   -- x y pos
  , dims   :: (V2 Unit) } -- width and height
  deriving Show

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

aabbCheck :: AABB -> AABB -> Bool
aabbCheck (AABB (V2 x1 y1) (V2 w1 h1)) (AABB (V2 x2 y2) (V2 w2 h2)) =
  not ( (x1 + w1) < x2
      || x1 > (x2 + w2)
      || (y1 + h1) < y2
      || y1 > (y2 + h2) )

-- aabbMin :: AABB -> V2 Unit
-- aabbMin a =
--   let (V2 x y) = center a
--       (V2 w h) = dims   a
--   in V2 (x - (w / 2)) (y - (h / 2))

-- aabbMax :: AABB -> V2 Unit
-- aabbMax a =
--   let (V2 x y) = center a
--       (V2 w h) = dims   a
--   in V2 (x + (w / 2)) (y + (h / 2))

-- minkowskiDiff :: AABB -> AABB -> AABB
-- minkowskiDiff a b =
--   let topLeft  = (aabbMin a)  - (aabbMax b)
--       fullSize = (dims a) + (dims b)
--   in AABB (topLeft + (fullSize / 2)) fullSize

penetrationVector :: AABB -> AABB -> V2 Unit
penetrationVector (AABB (V2 x1 y1) (V2 w1 h1)) (AABB (V2 x2 y2) (V2 w2 h2)) =
  let xV = (x1 + (w1 / 2)) - (x2 + (w2 / 2))
      yV = (y1 + (h1 / 2)) - (y2 + (h2 / 2))
      hW = (w1 / 2) + (w2 / 2)
      hH = (h1 / 2) + (h2 / 2)
      oX = hW - abs xV
      oY = hH - abs yV
      vec = V2 oX oY
  in if (oX >= oY)
     then if (yV > 0)
          then vec * (toVector TopN)
          else vec * (toVector BottomN)
     else if (xV > 0)
          then vec * (toVector RightN)
          else vec * (toVector LeftN)

toVector :: CNormal -> V2 Unit
toVector LeftN   = V2 (-1)  0
toVector RightN  = V2   1   0
toVector TopN    = V2   0   1
toVector BottomN = V2   0 (-1)
toVector NoneN   = V2   0   0

broadPhaseAABB :: BoundingBox -> Position -> Velocity -> AABB
broadPhaseAABB (BoundingBox (V2 w  h ))
               (Position    (V2 x  y ))
               (Velocity    (V2 vx vy)) =
  let xMod = vx * Unit dTinSeconds
      yMod = vy * Unit dTinSeconds
      x' = if vx > 0 then x        else x + xMod
      y' = if vy > 0 then y        else y + yMod
      w' = if vx > 0 then w + xMod else w - xMod
      h' = if vy > 0 then h + yMod else h - yMod
  in AABB { center = (V2 x' y'), dims = (V2 w' h') }

sweepAABB :: Velocity -> AABB -> AABB -> (Double, CNormal)
sweepAABB (Velocity (V2 (Unit b1vx) (Unit b1vy)))
          (AABB (V2 (Unit b1x) (Unit b1y)) (V2 (Unit b1w) (Unit b1h)))
          (AABB (V2 (Unit b2x) (Unit b2y)) (V2 (Unit b2w) (Unit b2h))) =
  let (xInvEntry, xInvExit) = getInvX
      (yInvEntry, yInvExit) = getInvY
      (xEntry, xExit) = getXThrough xInvEntry xInvExit
      (yEntry, yExit) = getYThrough yInvEntry yInvExit
      entryTime = max xEntry yEntry
      exitTime  = min xExit  yExit
   in
     if (entryTime > exitTime || xEntry < 0 && yEntry < 0 || xEntry > 1 || yEntry > 1)
     then (1, NoneN) -- no collision
     else if (xEntry > yEntry)
          then if (xInvEntry < 0)
               then (entryTime, LeftN)
               else (entryTime, RightN)
          else if (yInvEntry < 0)
               then (entryTime, TopN)
               else (entryTime, BottomN)
    where
      getInvX =
        if b1vx > 0
        then (b2x - (b1x + b1w), (b2x + b2w) - b1x)
        else ((b2x + b2w) - b1x, b2x - (b1x + b1w))
      getInvY =
        if b1vy > 0
        then (b2y - (b1y + b1h), (b2y + b2h) - b1y)
        else ((b2y + b2h) - b1y, b2y - (b1y + b1h))
      getXThrough xInvEntry xInvExit =
        if b1vx == 0
        then (-10000000, 10000000)
        else ( xInvEntry / (b1vx * dTinSeconds)
             , xInvExit  / (b1vx * dTinSeconds) )
      getYThrough yInvEntry yInvExit =
        if b1vy == 0
        then (-10000000, 10000000)
        else ( yInvEntry / (b1vy * dTinSeconds)
             , yInvExit  / (b1vy * dTinSeconds) )