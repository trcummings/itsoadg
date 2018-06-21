module Game.Util.RayCast where

import           Linear (V2(..), (*^), _x, _y)
import           Control.Lens
import           Data.Coerce (coerce)
import           Data.Maybe (catMaybes)
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Game.Util.AABB (aabbMin, aabbMax)
import           Game.Types
  ( AABB(..)
  , Ray(..)
  , RaycastHit(..)
  , Unit(..)
  , Position(..) )

sortByDistance :: [(RaycastHit, a)] -> [(RaycastHit, a)]
sortByDistance = sortBy (comparing (distance . fst))

rayIntersection2d :: Ray -> AABB -> Maybe RaycastHit
rayIntersection2d ray box =
  let bMin                     = aabbMin box
      bMax                     = aabbMax box
      scale                    = 1 / delta ray
      (V2 nearTimeX nearTimeY) = (bMin - origin ray) * scale
      (V2 farTimeX  farTimeY ) = (bMax - origin ray) * scale
      nearTime = max nearTimeX nearTimeY
      farTime  = min farTimeX  farTimeY
   in if nearTime > farTimeY || nearTimeY > farTimeX
      then Nothing
      else if nearTime >= 1 || farTime <= 0
           then Nothing
           else
             let hitTime = max 0 $ min 1 nearTime
                 normal  = if nearTimeX > nearTimeY
                           then V2 (signum $ scale ^. _x) 0
                           else V2 (signum $ scale ^. _y) 0
                 distance = hitTime *^ delta ray
                 position = origin ray + distance
             in Just $ RaycastHit { hitTime  = hitTime
                                  , distance = distance
                                  , position = position
                                  , normal   = normal }

raycast :: Position -> V2 Unit -> Unit -> [(AABB, a)] -> Maybe (RaycastHit, a)
raycast (Position pos) dir len boxes =
  let ray  = Ray { origin = pos
                 , delta  = len *^ dir }
      hits = sortByDistance $
             catMaybes $
             map (\(box, a) ->
                    let hit = rayIntersection2d ray box
                    in fmap (\h -> (h, a)) hit
                 ) boxes
  in if (length hits == 0) then Nothing else Just $ head hits

