import Reversible
import Gillespie

main = do 
  result <- runReversible $ MaxSteps 100000
  print result

