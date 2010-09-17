import Gillespie
import Data.Map
import Test.HUnit

main = runTestTT $ TestList tests

tests = [test0, test1, test2, test3, test4]

a = "A"
b = "B"

test0 = -- binding, same reactant, creation of product.
  let pData = fromList [(a, 10)]
      reaction = Reaction [a, a] [b] 1
      propensity' = propensity pData reaction
      pData' = updateParticleData reaction pData
      expectedPropensity = 45
      expectedPData = fromList [(a, 8), (b, 1)] in
  TestCase $ (expectedPropensity @=? propensity') >> (expectedPData @=? pData')

test1 = -- annihilation, different product
  let pData = fromList [(a, 30), (b, 10)]
      reaction = Reaction [a, b] [] 1
      propensity' = propensity pData reaction
      pData' = updateParticleData reaction pData
      expectedPropensity = 300
      expectedPData = fromList [(a, 29), (b, 9)] in
  TestCase $ (expectedPropensity @=? propensity') >> (expectedPData @=? pData')

test2 = -- unimolecular
  let pData = fromList [(a, 30), (b, 10)]
      reaction = Reaction [a] [b] 1
      propensity' = propensity pData reaction
      pData' = updateParticleData reaction pData
      expectedPropensity = 30
      expectedPData = fromList [(a, 29), (b, 11)] in
  TestCase $ (expectedPropensity @=? propensity') >> (expectedPData @=? pData')

test3 = -- decay
  let pData = fromList [(a, 30), (b, 10)]
      reaction = Reaction [b] [] 1
      pData' = updateParticleData reaction pData
      expectedPData = fromList [(a, 30), (b, 9)] in
  expectedPData ~=? pData'

test4 = -- unbinding
  let pData = fromList [(a, 30), (b, 10)]
      reaction = Reaction [b] [a, a] 1
      propensity' = propensity pData reaction
      pData' = updateParticleData reaction pData
      expectedPropensity = 10
      expectedPData = fromList [(a, 32), (b, 9)] in
  TestCase $ (expectedPropensity @=? propensity') >> (expectedPData @=? pData')

