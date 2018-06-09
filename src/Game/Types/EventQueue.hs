module Game.Types.EventQueue where

data EventQueue = EventQueue
  { pastEvents    :: [Int]
  , currentEvents :: [Int] }
