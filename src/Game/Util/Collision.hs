module Game.Util.Collision where

import Linear (V2(..), (^*), (^/))
import Apecs (Entity)

import Game.Types
  ( Unit(..)
  , Position(..)
  , Velocity(..)
  , CollisionNormal(..)
  , CollisionTime(..)
  , PenetrationVector(..)
  , Collision(..) )
import Game.Constants (frameDeltaSeconds)

inverseNormal :: CollisionNormal -> CollisionNormal
inverseNormal LeftNormal   = RightNormal
inverseNormal RightNormal  = LeftNormal
inverseNormal TopNormal    = BottomNormal
inverseNormal BottomNormal = TopNormal
inverseNormal NoneNormal   = NoneNormal

toVector :: CollisionNormal -> V2 Unit
toVector LeftNormal   = V2 (-1)  0
toVector RightNormal  = V2   1   0
toVector TopNormal    = V2   0   1
toVector BottomNormal = V2   0 (-1)
toVector NoneNormal   = V2   0   0

resolveBaseCollision :: (CollisionTime, CollisionNormal) -> Velocity -> Velocity
resolveBaseCollision
  (CollisionTime collisionTime, normal)
  (Velocity (V2 vx vy)) =
    let remainingTime = frameDeltaSeconds * (1 - collisionTime)
        (V2 normalX normalY) = toVector normal
        dotProd = ((vx * normalY) + (vy * normalX)) * Unit remainingTime
        v =
          if (normal == TopNormal || normal == BottomNormal)
          then V2 vx (dotProd * normalX)
          else V2 (dotProd * normalY) vy
    in Velocity v

resolveNormalVelocity :: Velocity
                      -> PenetrationVector
                      -> CollisionNormal
                      -> Velocity
resolveNormalVelocity (Velocity v@(V2 vx vy)) (PenetrationVector pVector) normal =
  let v' = case normal of
             NoneNormal   -> v + (pVector ^/ Unit frameDeltaSeconds)
             TopNormal    -> V2 vx 0
             BottomNormal -> V2 vx 0
             LeftNormal   -> V2 0 vy
             RightNormal  -> V2 0 vy
  in Velocity v'