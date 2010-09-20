module Gillespie where

-- http://www.caam.rice.edu/~caam210/reac/lec.html 

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Random as R

-- Data types.
data Reaction = Reaction
  {reactants :: [Species],
   products :: [Species],
   rate :: Rate}
  deriving Show

type Reactions = [Reaction]
type Rate = Float
type Propensity = Float
type Species = String
type Copies = Int
type ParticleData = M.Map Species Copies
type Steps = Int
type Time = Float
data StopCondition = MaxTime Time | MaxSteps Steps
type Random = Float

data CurrentState = CurrentState {
    rng :: !R.StdGen,
    particleData :: !ParticleData,
    steps :: !Steps,
    time :: !Time
  } deriving Show

-- Initialize and start main loop.
gillespie :: ParticleData -> Reactions -> StopCondition -> IO (CurrentState)
gillespie particleData reactions stopCondition = do
  rng <- R.getStdGen
  let initialState = CurrentState rng particleData 0 0
  return $ mainLoop initialState reactions stopCondition

-- Main loop.
mainLoop :: CurrentState -> Reactions -> StopCondition -> CurrentState
mainLoop state reactions stopCondition =
  let next = mainLoop (updateState state reactions) reactions stopCondition
      stop = state in
  case (sum . M.elems. particleData) state of
    0 -> stop
    _ -> case stopCondition of
      MaxSteps maxSteps -> if steps state < maxSteps then next else stop
      MaxTime maxTime -> if time state < maxTime then next else stop

-- Execute 1 Gillespie step.
updateState :: CurrentState -> Reactions -> CurrentState
updateState state reactions = 
  let (r1, gen') = drawRandom (rng state) 
      (r2, gen'') = drawRandom (rng state {rng = gen'})
      (reaction, a0) = drawReaction r2 state reactions
      time' = time state + drawTime r1 a0  
      particleData' = updateParticleData reaction (particleData state)
      steps' = steps state + 1 in
  CurrentState gen'' particleData' steps' time'

-- Draw a random number in the interval (0, 1). So exclude the 0.
drawRandom :: R.StdGen -> (Random, R.StdGen)
drawRandom generator =
  let (r, generator') = R.random generator in
  if r == 0.0
    then drawRandom generator'
    else (r, generator') 

-- Draw next reaction given a random number, the current state of the system 
-- and a list of reactions. Also return a0.
drawReaction :: Random -> CurrentState -> Reactions -> (Reaction, Propensity)
drawReaction r state reactions =
   let propensities = map (propensity (particleData state)) reactions
       accumulatedPropensities = scanl1 (+) propensities 
       a0 = last accumulatedPropensities
       Just index = L.findIndex ((r * a0) <=) accumulatedPropensities
       reaction = reactions !! index in
  (reaction, a0)

-- Draw next reaction time given a random number and a0.
drawTime :: Random -> Propensity -> Time
drawTime r a0 = log r / (- a0)

-- Update particleData given a reaction.
updateParticleData :: Reaction -> ParticleData -> ParticleData
updateParticleData reaction =
  let reactantUpdaters = map (M.adjust (\n -> n - 1)) $ reactants reaction
      -- Insert product if it does not exists yet, update otherwise.
      productUpdater = \key -> M.insertWith (\_ n -> n + 1) key 1
      productUpdaters = map productUpdater $ products reaction in
  foldl1 (.) (reactantUpdaters ++ productUpdaters)

-- Calculate propensity aj.
propensity :: ParticleData -> Reaction -> Propensity
propensity particleData reaction = 
  let copies = copyNumbers particleData (reactants reaction)
      combinations = combinationsOfParticles (reactants reaction) copies
  in combinations * (rate reaction)

-- Given a mapping between species and number of particles and a list of (not 
-- necessarily unique) species, return a list with the number of particles for 
-- each species.
copyNumbers :: ParticleData -> [Species] -> [Copies]
copyNumbers particleData =
  map $ \species -> M.findWithDefault 0 species particleData

-- Given a list of species and a list with the number of particles for each 
-- species, compute the number of distinct combinations of particles.
combinationsOfParticles :: [Species] -> [Copies]  -> Float
combinationsOfParticles species copies = 
  if length species == 2 && (species!!0 == species!!1)
    then fromIntegral (copies!!0 * (copies!!1 - 1)) / 2
    else fromIntegral $ product copies
