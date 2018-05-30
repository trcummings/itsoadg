module Game.Step where

data Step a
  = Step'Change a a -- | Prev, Next
  | Step'Sustain a
  deriving (Show, Eq)

smash :: Step a -> a
smash (Step'Change _ a) = a
smash (Step'Sustain  a) = a

peel :: Step a -> a
peel (Step'Change a _) = a
peel (Step'Sustain  a) = a
