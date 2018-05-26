module Game.Camera where

import Linear (V2(..), (*^))
import Apecs (get, set, cmapM_)

import Game.World (System')
import Game.Types
  ( Camera(..), size, ppos
  , CameraTarget(..)
  , Acceleration(..)
  , Position(..) )

stepCamera :: System' ()
stepCamera = do
  -- update camera position based on target
  cmapM_ $ \(
      Camera s@(V2 cw ch) cp
    , CameraTarget e
    , Acceleration a
    , Position cpos
    , camera ) -> do

    (Position targetP) <- get e :: System' (Position)
        -- target x y based on camera size
    let txy = targetP - V2 (0.5 * cw) (0.5 * ch)
        -- camera acceleration towards target
        a'   = a + (txy - cpos)
        -- ppos with drag
        ppos' = cpos + (0.5 *^ (cp - cpos))
        -- verlet on cpos
        cpos' = cpos + (0.256 *^ a')
        -- differentiate to get new cpos
        d     = (2 *^ cpos') - ppos'
    -- set new values
    set camera (
        Camera { size = s, ppos = cpos' }
      , Acceleration $ V2 0 0
      , Position d )
