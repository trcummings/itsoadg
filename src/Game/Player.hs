{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Player where

-- import           Data.Text
-- import qualified Animate

-- import           Game.Types (PlayerKey(..))
-- -- import           Game.Constants (Seconds(..), frameDeltaSeconds)
-- -- import           Game.Step (Step(..))

-- playerKey'keyName :: PlayerKey -> Text
-- playerKey'keyName = \case
--   PlayerKey'RWalk -> "RWalk"
--   PlayerKey'RJump -> "RJump"
--   PlayerKey'RIdle -> "RIdle"
--   PlayerKey'LWalk -> "LWalk"
--   PlayerKey'LJump -> "LJump"
--   PlayerKey'LIdle -> "LIdle"

-- stepPlayerFramePosition :: Step PlayerAction
--                         -> Animations PlayerKey
--                         -> Animate.Position PlayerKey Seconds
--                         -> Animate.Position PlayerKey Seconds
-- stepPlayerFramePosition (Step'Sustain _) animations pos =
--   Animate.stepPosition animations pos (realToFrac frameDeltaSeconds)
-- stepPlayerFramePosition (Step'Change _ pA) _ _ = case pA of
