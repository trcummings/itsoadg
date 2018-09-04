module Game.Effect.Clock where

import SDL.Time (ticks)
import Apecs (get, set, global)
import Control.Monad.IO.Class (liftIO)

import Game.Types (Clock(..), PhysicsTime(..), GlobalTime(..))
import Game.Util.Constants (dT)
import Game.World.TH (ECS)

-- getFixedTime' :: HasGameState m => m (Double, Double)
-- getFixedTime' = do
--   PhysicsTime t acc <- _physicsTime <$> getGameState
--   return (t, acc)
--
-- getGlobalTime' :: ECS Double
-- getGlobalTime' = do
--   GlobalTime gt <- _globalTime <$> get global
--   return gt

accumulateFixedTime :: ECS ()
accumulateFixedTime = do
  -- get next time tick from SDL
  nextTime <- liftIO (fromIntegral <$> ticks :: IO Double)
  -- pull game state
  clock    <- get global :: ECS Clock
  let GlobalTime cTime = _globalTime  clock
      pt               = _physicsTime clock
  -- clamp frameTime at 25ms
  set global (clock { _globalTime  = GlobalTime nextTime
                    , _physicsTime = pt { accum = (accum pt)
                                                + (min 25 $ nextTime - cTime) } })

stepFixedTime :: Clock -> Clock
stepFixedTime clock =
  let pt = _physicsTime clock
  in clock { _physicsTime = PhysicsTime { time  = (time  pt + dT)
                                        , accum = (accum pt - dT) } }
