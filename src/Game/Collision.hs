module Game.Collision where

import Linear (V2(..), (^*), (^/))
import Apecs (Entity)

import Game.Types (Unit(..), Position(..), Velocity(..))
import Game.Constants (frameDeltaSeconds)

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

resolveBaseCollision :: Collision -> (Position, Velocity) -> (Position, Velocity)
resolveBaseCollision
  c@(Collision collisionTime normal _)
  (Position p, Velocity v@(V2 vx vy)) =
    let cTime = frameDeltaSeconds * collisionTime
        remainingTime = frameDeltaSeconds * (1 - collisionTime)
        vNormal@(V2 normalX normalY) = toVector normal
        dotProd = ((vx * normalY) + (vy * normalX)) * Unit remainingTime
        v' =
          if (normal == TopN || normal == BottomN)
          then V2 vx (dotProd * normalX)
          else V2 (dotProd * normalY) vy
        p' = p + v ^* Unit cTime
    in (Position p', Velocity v')

resolveNormalVelocity :: Velocity -> V2 Unit -> CNormal -> Velocity
resolveNormalVelocity (Velocity v@(V2 vx vy)) pVector normal =
  let v' = case normal of
             NoneN   -> v + (pVector ^/ Unit frameDeltaSeconds)
             TopN    -> V2 vx 0
             BottomN -> V2 vx 0
             LeftN   -> V2 0 vy
             RightN  -> V2 0 vy
  in Velocity v'

