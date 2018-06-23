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
  , Velocity(..)
  , Axis(..)
  , SensorDirection(..) )

getRaycastDir :: Axis -> Velocity -> V2 Unit
getRaycastDir X (Velocity v) = signum <$> v * V2 1 0
getRaycastDir Y (Velocity v) = signum <$> v * V2 0 1

getRaycastPos :: SensorDirection -> Velocity -> AABB -> V2 Unit
getRaycastPos Sensor'Top    (Velocity (V2 vx _)) box =
  if vx > 0
  then aabbMin        box
  else aabbTopRight   box
getRaycastPos Sensor'Bottom (Velocity (V2 vx _)) box =
  if vx > 0
  then aabbMax        box
  else aabbBottomLeft box
getRaycastPos Sensor'Right  (Velocity (V2 _ vy)) box =
  if vy > 0
  then aabbMax        box
  else aabbTopRight   box
getRaycastPos Sensor'Left   (Velocity (V2 _ vy)) box =
  if vy > 0
  then aabbMin        box
  else aabbBottomLeft box

getRaycastPosOffset :: SensorDirection -> Velocity -> V2 Unit
getRaycastPosOffset Sensor'Top    (Velocity (V2 vx _)) =
  if vx > 0
  then V2 (-onePixel)   0
  else V2   onePixel    0
getRaycastPosOffset Sensor'Bottom (Velocity (V2 vx _)) =
  if vx > 0
  then V2   onePixel  (-onePixel)
  else V2 (-onePixel) (-onePixel)
getRaycastPosOffset Sensor'Right  (Velocity (V2 _ vy)) =
  if vy > 0
  then V2 (-onePixel)  onePixel
  else V2 (-onePixel) (-onePixel)
getRaycastPosOffset Sensor'Left   (Velocity (V2 _ vy)) =
  if vy > 0
  then V2   onePixel  (-onePixel)
  else V2   onePixel    onePixel

getRaycastLength :: Velocity -> V2 Unit -> V2 Unit
getRaycastLength (Velocity v) normal = normal * v ^* Unit frameDeltaSeconds

numRays :: Unit -> Unit
numRays x =
  let x' = coerce x :: Double
  -- if whole number
  in if (x' == fromInteger (round x'))
  then Unit $ (2 * x') + 1
  else Unit $ (fromInteger $ ceiling x') * 2

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
                           else V2 0 (signum $ scale ^. _y)
                 distance = hitTime *^ delta ray
                 position = origin ray + distance
             in Just $ RaycastHit { hitTime   = hitTime
                                  , rayOrigin = origin ray
                                  , distance  = distance
                                  , position  = position
                                  , normal    = normal }

sortByDistance :: [(RaycastHit, a)] -> [(RaycastHit, a)]
sortByDistance = sortBy (comparing (distance . fst))

raycast :: V2 Unit -> V2 Unit -> [(AABB, a)] -> Maybe (RaycastHit, a)
raycast pos delta boxes =
  let ray  = Ray { origin = pos
                 , delta  = delta }
      hits = sortByDistance $
             catMaybes $
             map (\(box, a) ->
                    let hit = rayIntersection2d ray box
                    in fmap (\h -> (h, a)) hit
                 ) boxes
  in if (length hits == 0) then Nothing else Just $ head hits

