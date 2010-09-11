import Gillespie
import Data.Map

main = do
  result <- runGillespie particleMap reactions stopCondition
  putStrLn $ show result

-- Species.
a = "A"
b = "B"

-- Number of particles.
particleMap = fromList [(a, 300), (b, 100)]

-- Reactions.
reactions = 
  [Reaction [a] [b] 2,
   Reaction [b] [a, a] 2]

-- Stop condition.
stopCondition = Steps 10
