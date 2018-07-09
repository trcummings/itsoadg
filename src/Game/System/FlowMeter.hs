module Game.System.FlowMeter where

import           Game.Util.Constants (frameDeltaSeconds)
import           Game.Types (FlowMeter(..))
import           Game.Wrapper.Apecs (Apecs(..))

recoveryPerSecond :: Double
recoveryPerSecond = 2.5

tickUp :: Double
tickUp = recoveryPerSecond * frameDeltaSeconds

updateFlowMeter :: FlowMeter -> FlowMeter
updateFlowMeter flow =
  let ct = counter flow
      cf = currentFlow flow
      bf = baseFlow flow
  -- if we are above the baseline flow
  in if cf >= bf
     -- reset recovery counter
     then flow { counter = 0 }
     -- otherwise, we are below baseline flow so,
     -- decide how to progress with recovery
     else if ct >= 1
          -- bump counter to start ticking to recovery
          then flow { counter = ct + frameDeltaSeconds }
          -- bump current flow to a max of baseFlow
          else flow { currentFlow = min bf (cf + tickUp) }

stepFlowMeter :: Apecs m => m ()
stepFlowMeter = cmap $ \(flow@(FlowMeter _ _ _ _)) -> updateFlowMeter flow
