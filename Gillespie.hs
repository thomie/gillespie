module Gillespie where

import qualified Data.Map as M
import System.Random


-- Data types.
data Reaction = Reaction
  {reactants :: [Species],
   products :: [Species],
   rate :: Float}
  deriving Show

type Reactions = [Reaction]
type Species = String
type Steps = Int
type Time = Float
type NParticles = M.Map Species Int

data StopCondition = Time Float | Steps Int

data CurrentState = CurrentState {
    rng_ :: StdGen,
    particleMap_ :: NParticles,
    steps_ :: Steps,
    time_ :: Time,
    randoms_ :: [Int]
  } deriving Show


-- Initialize and start main loop.
runGillespie :: NParticles -> Reactions -> StopCondition -> IO (CurrentState)
runGillespie particleMap reactions stopCondition = do
  initialState <- initialize particleMap
  return $ mainLoop initialState reactions stopCondition


-- Initalize the random number generator, steps and time.
initialize :: NParticles -> IO CurrentState
initialize nParticles = do
  rng <- getStdGen
  return $ CurrentState rng nParticles 0 0 []


-- Main loop.
mainLoop :: CurrentState -> Reactions -> StopCondition -> CurrentState
mainLoop state reactions stopCondition =
  case countParticles state of
    0 -> state
    _ -> case stopCondition of
      Steps stop -> 
        if steps_ state < stop 
          then mainLoop (step state) reactions stopCondition
          else state
      Time stop ->
        if time_ state < stop 
          then mainLoop (step state) reactions stopCondition
          else state


-- One Gillespie step.
step :: CurrentState -> CurrentState
step state = 
  let (val, gen') = random (rng_ state)
      particleMap = particleMap_ state
      steps = steps_ state
      time = time_ state
      randoms = randoms_ state
      (particleMap', time') = update val particleMap time
  in
  CurrentState gen' particleMap' (steps + 1) time' (val:randoms)

update :: Int -> NParticles -> Time -> (NParticles, Time)
update _ particleMap time = (particleMap, time)


-- Count total number of particles.
countParticles state = sum $ M.elems $ particleMap_ state


