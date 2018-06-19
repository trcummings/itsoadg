module Game.Util.RayCast where

import           Linear (V2(..), (*^))
import           Data.Coerce (coerce)
import           Data.Maybe (catMaybes)
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Game.Util.AABB (aabbMin, aabbMax)
import           Game.Types
  ( AABB(..)
  , RaycastHit(..)
  , Unit(..)
  , Position(..) )

checkIntersection :: Unit -> Unit -> Unit -> Unit -> Maybe Unit
checkIntersection o d i h =
  if (d == 0 && not (o < i || o > h))
  then Nothing
  else
    let t1 = (i - o) / d
        t2 = (h - o) / d
    in if t1 > t2
       then
         let tNear = t2
             tFar  = t1
         in if (tNear > tFar || tFar < 0) then Nothing else Just tNear
       else
         let tNear = t1
             tFar  = t2
         in if (tNear > tFar || tFar < 0) then Nothing else Just tNear

sortByDistance :: [(RaycastHit, a)] -> [(RaycastHit, a)]
sortByDistance = sortBy (comparing (distance . fst))

castRayOnBox :: Position -> V2 Unit -> Unit -> AABB -> Maybe RaycastHit
castRayOnBox (Position origin) direction len box =
  let (V2 xMin yMin) = aabbMin box
      (V2 xMax yMax) = aabbMax box
      (V2 dX dY)     = origin + (len *^ direction)
      (V2 oX oY)     = origin
      iX = checkIntersection oX dX xMin xMax
      iY = checkIntersection oY dY yMin yMax
   in case iX of
        Nothing  -> Nothing
        Just iX' ->
          case iY of
            Nothing  -> Nothing
            Just iY' ->
               let distance = Unit $ sqrt $
                     ((coerce $ ((oX - iX') ** 2) + ((oY - iY') ** 2)) :: Double)
                   xNormal  = if oX < xMin
                              then (-1)
                              else if oX > xMax then 1 else 0
                   yNormal  = if oY < yMin
                              then (-1)
                              else if oY > yMax then 1 else 0
               in Just $ RaycastHit
                  { distance = distance
                  , fraction = distance / len
                  , position = V2 iX' iY'
                  , normal   = V2 xNormal yNormal }

raycast :: Position -> V2 Unit -> Unit -> [(AABB, a)] -> Maybe (RaycastHit, a)
raycast pos dir len boxes =
  let hits = sortByDistance $
             catMaybes $
             map (\(box, a) ->
                    let hit = castRayOnBox pos dir len box
                    in fmap (\h -> (h, a)) hit
                 ) boxes
  in if (length hits == 0) then Nothing else Just $ head hits

