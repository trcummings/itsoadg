module Game.System.FixedTime where

import           Apecs (global)

import           Game.Util.Constants (dT)
import           Game.Types
  ( PhysicsTime(..), time, accum
  , GlobalTime(..) )
import           Game.Wrapper.Apecs (Apecs(..))

getFixedTime :: Apecs m => m (Double, Double)
getFixedTime = do
  PhysicsTime t acc <- get global
  return (t, acc)

accumulateFixedTime :: Apecs m => Double -> m ()
accumulateFixedTime nextTime = do
  GlobalTime currentTime <- get global
  -- update global time
  cmap $ \(GlobalTime _) -> GlobalTime nextTime
  -- update physics frame time accumulator
  cmap $ \(PhysicsTime t acc) -> PhysicsTime
    { time = t
    -- clamp frameTime at 25ms
    , accum = acc + (min 25 $ nextTime - currentTime) }

clearFixedTime :: Apecs m => m ()
clearFixedTime = cmap $ \(PhysicsTime t' acc') ->
  PhysicsTime { time = (t' + dT), accum = (acc' - dT) }
