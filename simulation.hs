import Gillespie
import Data.Map

main = gillespie pData reactions stopCondition

-- Species.
a = "A"
b = "B"

-- Number of particles per species.
-- Steady state is (300, 100).
pData = fromList [(a, 100), (b, 200)]

-- Reactions.
reactions = 
  [Reaction [a, a] [b] 0.1,
   Reaction [b] [a, a] 45.0]

-- Stop condition.
stopCondition = Time 1
