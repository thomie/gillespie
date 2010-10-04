import Gillespie
import Data.Map

main = do
  result <- gillespie pData reactions stopCondition
  print result

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

-- Stop condition.
--stopCondition = MaxTime 10
stopCondition = MaxSteps 100000
