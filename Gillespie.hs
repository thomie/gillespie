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
type Copies = Integer
type ParticleData = M.Map Species Copies
type Steps = Integer
type Time = Float
data StopCondition = Time Float | Steps Integer
type Random = Float

data CurrentState = CurrentState {
    rng :: R.StdGen,
    particleData :: ParticleData,
    steps :: Steps,
    time :: Time
  } deriving Show

-- Initialize and start main loop.
gillespie :: ParticleData -> Reactions -> StopCondition -> IO (CurrentState)
gillespie particleData reactions stopCondition = do
  initialState <- initialize particleData
  return $ mainLoop initialState reactions stopCondition

-- Initalize the random number generator, steps and time.
initialize :: ParticleData -> IO CurrentState
initialize nParticles = do
  rng <- R.getStdGen
  return $ CurrentState rng nParticles 0 0

-- Main loop.
mainLoop :: CurrentState -> Reactions -> StopCondition -> CurrentState
mainLoop state reactions stopCondition =
  let next = mainLoop (updateState state reactions) reactions stopCondition
      stop = state in
  case (sum . M.elems. particleData) state of
    0 -> stop
    _ -> case stopCondition of
      Steps maxSteps -> if steps state < maxSteps then next else stop
      Time maxTime -> if time state < maxTime then next else stop

-- Update state after 1 Gillespie step.
updateState :: CurrentState -> Reactions -> CurrentState
updateState state reactions = 
  let (r1, gen') = R.random (rng state)
      (r2, gen'') = R.random (rng state {rng = gen'})
      (particleData', time') = step r1 r2 state reactions
      steps' = steps state + 1 
  in
  CurrentState gen'' particleData' steps' time'

-- Execute one Gillespie step.
step :: Random -> Random -> CurrentState -> Reactions -> (ParticleData, Time)
step r1 r2 state reactions =
  let (reaction, a0) = drawReaction r2 state reactions
      time' = time state + drawTime r1 a0  
      particleData' = updateParticleData reaction (particleData state) in
  (particleData', time')

-- Draw next reaction given a random number, the current state of the system 
-- and a list of reactions. Also return a0.
drawReaction :: Random -> CurrentState -> Reactions -> (Reaction, Propensity)
drawReaction r state reactions =
   let propensities = map (propensity (particleData state)) reactions
       accumulatedPropensities = scanl1 (+) propensities 
       a0 = last accumulatedPropensities
       Just index = L.findIndex ((r * a0) <) accumulatedPropensities
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
