module Game.Effect.Clock where

import Game.Types (GameState(..), PhysicsTime(..), GlobalTime(..))
import Game.Wrapper.SDLTime (SDLTime(..))
import Game.Effect.HasGameState (HasGameState(..))
import Game.Util.Constants (dT)

class Monad m => Clock m where
  getFixedTime        :: m (Double, Double)
  getGlobalTime       :: m Double
  accumulateFixedTime :: m ()
  clearFixedTime      :: m ()

getFixedTime' :: HasGameState m => m (Double, Double)
getFixedTime' = do
  PhysicsTime t acc <- _physicsClock <$> getGameState
  return (t, acc)

getGlobalTime' :: HasGameState m => m Double
getGlobalTime' = do
  GlobalTime gt <- _globalClock <$> getGameState
  return gt

accumulateFixedTime' :: (HasGameState m, SDLTime m) => m ()
accumulateFixedTime' = do
  -- get next time tick from SDL
  nextTime <- nextTick
  -- pull game state
  setGameState $ \gs ->
    let GlobalTime cTime = _globalClock gs
        pt               = _physicsClock gs
    in gs { _globalClock  = GlobalTime nextTime
          -- clamp frameTime at 25ms
          , _physicsClock = pt { accum = (accum pt) + (min 25 $ nextTime - cTime) } }

stepFixedTime :: PhysicsTime -> PhysicsTime
stepFixedTime pt = PhysicsTime { time  = (time  pt + dT)
                               , accum = (accum pt - dT) }

clearFixedTime' :: HasGameState m => m ()
clearFixedTime' = setGameState $ \gs ->
  gs { _physicsClock = stepFixedTime $ _physicsClock gs }
