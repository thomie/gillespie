import Gillespie
import Data.Map
import Test.HUnit

main = runTestTT $ TestList tests

tests =
    [ test0
    , test1
    , test2
    , test3
    , test4
    ]

a = "A"
b = "B"

test0 = -- Binding reaction: same reactant, creation of product.
    TestCase $
        (expectedPropensity @=? propensity') >>
        (expectedPData @=? pData')
  where
    pData = fromList [(a, 10)]
    reaction = createReaction [a, a] [b] 1
    propensity' = propensity pData reaction
    pData' = updateParticleData reaction pData
    expectedPropensity = 45
    expectedPData = fromList [(a, 8), (b, 1)]

test1 = -- Annihilation reaction: different reactant.
    TestCase $
        (expectedPropensity @=? propensity') >>
	(expectedPData @=? pData')
  where
    pData = fromList [(a, 30), (b, 10)]
    reaction = createReaction [a, b] [] 1
    propensity' = propensity pData reaction
    pData' = updateParticleData reaction pData
    expectedPropensity = 300
    expectedPData = fromList [(a, 29), (b, 9)]

test2 = -- Unimolecular reaction.
    TestCase $
        (expectedPropensity @=? propensity') >>
	(expectedPData @=? pData')
  where
    pData = fromList [(a, 30), (b, 10)]
    reaction = createReaction [a] [b] 1
    propensity' = propensity pData reaction
    pData' = updateParticleData reaction pData
    expectedPropensity = 30
    expectedPData = fromList [(a, 29), (b, 11)]

test3 = -- Decay reaction.
    expectedPData ~=? pData'
  where
    pData = fromList [(a, 30), (b, 10)]
    reaction = createReaction [b] [] 1
    pData' = updateParticleData reaction pData
    expectedPData = fromList [(a, 30), (b, 9)]

test4 = -- Unbinding reactino.
    TestCase $
        (expectedPropensity @=? propensity') >>
	(expectedPData @=? pData')
  where
    pData = fromList [(a, 30), (b, 10)]
    reaction = createReaction [b] [a, a] 1
    propensity' = propensity pData reaction
    pData' = updateParticleData reaction pData
    expectedPropensity = 10
    expectedPData = fromList [(a, 32), (b, 9)]

