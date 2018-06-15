module Game.System.FixedTime where

import           Apecs (cmap, get, global)

import           Game.World (System')
import           Game.Util.Constants (dT)
import           Game.Types
  ( PhysicsTime(..), time, accum
  , GlobalTime(..) )

getFixedTime :: System' (Double, Double)
getFixedTime = do
  PhysicsTime t acc <- get global
  return (t, acc)

accumulateFixedTime :: Double -> System' ()
accumulateFixedTime nextTime = do
  GlobalTime currentTime <- get global
  -- update global time
  cmap $ \(GlobalTime _) -> GlobalTime nextTime

  -- update physics frame time accumulator
  cmap $ \(PhysicsTime t acc) -> PhysicsTime
    { time = t
    -- clamp frameTime at 25ms
    , accum = acc + (min 25 $ nextTime - currentTime) }

clearFixedTime :: System' ()
clearFixedTime = cmap $ \(PhysicsTime t' acc') ->
  PhysicsTime { time = (t' + dT), accum = (acc' - dT) }
