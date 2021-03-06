module Gillespie where

-- http://www.caam.rice.edu/~caam210/reac/lec.html 

import qualified Data.List as L
import qualified Data.Map as M
import System.Random.Mersenne.Pure64 as R

-- Data types.
data Reaction = Reaction
  { reactants :: Reactants
  , products :: Products
  , rate :: Rate
  , updateParticleData :: (ParticleData -> ParticleData)
  }

type Reactions = [Reaction]
type Rate = Double
type Propensity = Double
type Species = String
type Reactants = [Species]
type Products = [Species]
type Copies = Int
type ParticleData = M.Map Species Copies
type Steps = Int
type Time = Double
data StopCondition = MaxTime Time | MaxSteps Steps
type RandomDouble = Double

data CurrentState = CurrentState
    { rng :: !R.PureMT
    , particleData :: !ParticleData
    , steps :: !Steps
    , time :: !Time
    } deriving Show

-- Create a Reaction.
createReaction :: Reactants -> Products -> Rate -> Reaction
createReaction reactants products rate =
    Reaction reactants products rate $ particleDataUpdater reactants products

-- Create a function to update the particleData, given the list of reactants 
-- and the list of products of a reaction.
particleDataUpdater :: Reactants -> Products -> ParticleData -> ParticleData
particleDataUpdater reactants products =
    foldl1 (.) (reactantUpdaters ++ productUpdaters)
  where
    reactantUpdaters = map (M.adjust (\n -> n - 1)) reactants
    -- Insert product if it does not exists yet, update otherwise.
    productUpdater = \key -> M.insertWith (\_ n -> n + 1) key 1
    productUpdaters = map productUpdater products

-- Initialize and start main loop.
gillespie :: ParticleData -> Reactions -> StopCondition -> IO CurrentState
gillespie particleData reactions stopCondition = do
    --let rng = (R.pureMT 0)
    rng <- R.newPureMT
    let initialState = CurrentState rng particleData 0 0
    return $ mainLoop initialState reactions stopCondition

-- Main loop.
mainLoop :: CurrentState -> Reactions -> StopCondition -> CurrentState
mainLoop state reactions stopCondition =
    case (sum . M.elems . particleData) state of
        0 ->
	    stop
        _ ->
	    case stopCondition of
	        MaxSteps maxSteps ->
		    if steps state < maxSteps
			then next
			else stop
	        MaxTime maxTime ->
		    if time state < maxTime
			then next
			else stop
  where
    next = mainLoop (updateState state reactions) reactions stopCondition
    stop = state

-- Execute 1 Gillespie step.
updateState :: CurrentState -> Reactions -> CurrentState
updateState state reactions = 
    CurrentState generator' particleData' steps' time'
  where
    (r1, r2, generator') = drawTwoRandoms (rng state) 
    (reaction, a0) = drawReaction r2 state reactions
    time' = time state + drawTime r1 a0  
    particleData' = updateParticleData reaction (particleData state)
    steps' = steps state + 1

-- Draw a random number in the interval (0, 1). So exclude the 0.
drawTwoRandoms :: R.PureMT -> (RandomDouble, RandomDouble, R.PureMT)
drawTwoRandoms generator =
    if r1 == 0.0 || r2 == 0.0
        then drawTwoRandoms generator''
        else (r1, r2, generator'')
  where
    (r1, generator') = R.randomDouble generator
    (r2, generator'') = R.randomDouble generator'

-- Draw next reaction given a random number, the current state of the system 
-- and a list of reactions. Also return a0.
drawReaction :: RandomDouble -> CurrentState -> Reactions -> (Reaction, Propensity)
drawReaction r state reactions =
    (reaction, a0)
  where
    propensities = map ((propensity . particleData) state) reactions
    accumulatedPropensities = scanl1 (+) propensities 
    a0 = last accumulatedPropensities
    Just index = L.findIndex ((r * a0) <=) accumulatedPropensities
    reaction = reactions !! index

-- Draw next reaction time given a random number and a0.
drawTime :: RandomDouble -> Propensity -> Time
drawTime r a0 = log r / (- a0)

-- Calculate propensity aj.
propensity :: ParticleData -> Reaction -> Propensity
propensity particleData reaction = 
    combinations * (rate reaction)
  where
    copies = copyNumbers particleData (reactants reaction)
    combinations = combinationsOfParticles (reactants reaction) copies

-- Given a mapping between species and number of particles and a list of (not 
-- necessarily unique) species, return a list with the number of particles for 
-- each species.
copyNumbers :: ParticleData -> [Species] -> [Copies]
copyNumbers particleData =
    map $ \species -> M.findWithDefault 0 species particleData

-- Given a list of species and a list with the number of particles for each 
-- species, compute the number of distinct combinations of particles.
combinationsOfParticles :: [Species] -> [Copies]  -> Double
combinationsOfParticles species copies = 
    if length species == 2 && (species!!0 == species!!1)
        then fromIntegral (copies!!0 * (copies!!1 - 1)) / 2
        else fromIntegral $ product copies
