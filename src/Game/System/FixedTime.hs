{-# LANGUAGE ScopedTypeVariables #-}

module Game.System.FixedTime where

import           Apecs (global)

import           Game.Util.Constants (dT)
import           Game.Types (PhysicsTime(..), GlobalTime(..))
import           Game.Wrapper.Apecs (Apecs(..))

getFixedTime :: Apecs m => m (Double, Double)
getFixedTime = do
  PhysicsTime t acc <- get global
  return (t, acc)

-- clamp frameTime at 25ms
accumulateFixedTime :: Apecs m => Double -> m ()
accumulateFixedTime nextTime = do
  cmap $ \(GlobalTime cTime, pt :: PhysicsTime) ->
    ( GlobalTime nextTime
    , pt { accum = (accum pt) + (min 25 $ nextTime - cTime) } )

clearFixedTime :: Apecs m => m ()
clearFixedTime = cmap $ \(PhysicsTime t' acc') ->
  PhysicsTime { time = (t' + dT), accum = (acc' - dT) }
