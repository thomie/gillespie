module Reversible where

import Gillespie
import Data.Map

runReversible = gillespie pData reactions

-- Species.
a = "A"
b = "B"

-- Number of particles per species.
-- Steady state is (300, 100).
pData = fromList [(a, 100), (b, 200)]

-- Reactions.
reactions = 
  [createReaction [a, a] [b] 0.1,
   createReaction [b] [a, a] 45.0]

