module Game.Step where

import           Game.Types (Step(..) )

smash :: Step a -> a
smash (Step'Change _ a) = a
smash (Step'Sustain  a) = a

peel :: Step a -> a
peel (Step'Change a _) = a
peel (Step'Sustain  a) = a
