{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Util where

import           Data.Aeson (FromJSON(..), ToJSON(..))

-- Utility types
newtype Unit =
  Unit Double
  deriving (Eq, Ord, Show, Num, Fractional, Floating, Real, RealFrac, RealFloat)

newtype Seconds =
  Seconds Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

data Step a
  = Step'Change a a -- | Prev, Next
  | Step'Sustain a
  deriving (Show, Eq)
