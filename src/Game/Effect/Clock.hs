module Game.Effect.Clock where

import SDL.Time (ticks)
import Apecs (get, set, global)
import Control.Monad.IO.Class (liftIO)

import Game.Types (Clock(..), PhysicsTime(..), GlobalTime(..))
import Game.Util.Constants (dT)
import Game.World.TH (ECS)

getAccumulatedTime :: ECS Double
getAccumulatedTime = do
  clock <- (get global :: ECS Clock)
  return $ _accum $ _physicsTime clock

getGlobalTime :: ECS GlobalTime
getGlobalTime = do
  clock <- get global :: ECS Clock
  return $ _globalTime clock

clearFixedTime :: ECS ()
clearFixedTime = do
  clock <- get global :: ECS Clock
  set global $ stepFixedTime clock

accumulateFixedTime :: ECS ()
accumulateFixedTime = do
  -- get next time tick from SDL
  nextTime <- liftIO (fromIntegral <$> ticks :: IO Double)
  -- pull game state
  clock    <- get global :: ECS Clock
  let GlobalTime _ cTime = _globalTime  clock
      pt                 = _physicsTime clock
      accum'             = min 25 $ nextTime - cTime
  -- clamp frameTime at 25ms
  set global (clock { _globalTime  = GlobalTime cTime nextTime
                    , _physicsTime = pt { _accum = (_accum pt) + accum' } })

stepFixedTime :: Clock -> Clock
stepFixedTime clock =
  let pt = _physicsTime clock
  in clock { _physicsTime = PhysicsTime { _time  = (_time  pt + dT)
                                        , _accum = (_accum pt - dT) } }
