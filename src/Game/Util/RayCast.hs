module Game.Util.Raycast where

import           Linear (V2(..), (*^), (^*), _x, _y)
import           Control.Lens
import           Data.Coerce (coerce)
import           Data.Maybe (catMaybes)
import           Data.List (sortBy)
import           Data.Ord (comparing)

import           Game.Util.Constants (onePixel, frameDeltaSeconds)
import           Game.Util.AABB (aabbMin, aabbMax, aabbTopRight, aabbBottomLeft)
import           Game.Types
  ( AABB(..)
  , Ray(..)
  , RaycastHit(..)
  , Unit(..)
  , Position(..)
  , Velocity(..)
  , Axis(..)
  , SensorDirection(..) )

getRaycastDir :: Axis -> Velocity -> V2 Unit
getRaycastDir X (Velocity v) = signum <$> v * V2 1 0
getRaycastDir Y (Velocity v) = signum <$> v * V2 0 1

getRaycastPos :: SensorDirection -> Velocity -> AABB -> Position
getRaycastPos Sensor'Top    (Velocity (V2 vx _)) box = Position $
  if vx > 0
  then aabbMin        box + V2 (-onePixel)   onePixel
  else aabbTopRight   box + V2   onePixel    onePixel
getRaycastPos Sensor'Bottom (Velocity (V2 vx _)) box = Position $
  if vx > 0
  then aabbMax        box + V2   onePixel  (-onePixel)
  else aabbBottomLeft box + V2 (-onePixel) (-onePixel)
getRaycastPos Sensor'Right  (Velocity (V2 _ vy)) box = Position $
  if vy > 0
  then aabbMax        box + V2 (-onePixel)  onePixel
  else aabbTopRight   box + V2 (-onePixel) (-onePixel)
getRaycastPos Sensor'Left   (Velocity (V2 _ vy)) box = Position $
  if vy > 0
  then aabbMin        box + V2   onePixel  (-onePixel)
  else aabbBottomLeft box + V2   onePixel    onePixel

getRaycastLength :: Axis -> Velocity -> Unit
getRaycastLength axis (Velocity v) =
  let (V2 xL yL)  = v ^* Unit frameDeltaSeconds
  in case axis of X -> abs xL
                  Y -> abs yL

rayIntersection2d :: Ray -> AABB -> Maybe RaycastHit
rayIntersection2d ray box =
  let bMin                     = aabbMin box
      bMax                     = aabbMax box
      scale                    = V2 (1 / delta ray ^. _x) (1 / delta ray ^. _y)
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

sortByDistance :: [(RaycastHit, a)] -> [(RaycastHit, a)]
sortByDistance = sortBy (comparing (distance . fst))

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

