----
---- CNN-PhD version 0.1, Copyright (C) 5/Jun/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Tri-State Neural Network class
module EvolClass
-- (
-- )
where

import           Data.Bool (bool)
import           TNNClass
import           TState

data Sample = Sample {
    sInputs :: [TState],
    sOutput :: [TState]
} deriving (Show)

data Solver = Solver {
    sWeights  :: TState,
    sFunction :: TState -> TState -> TState
}

instance Show Solver where
    show (Solver srW _) = "Solver ShiftBy:" ++ show srW

data InnerMap = InnerMap {
    mapping  :: [TState],
    distance :: [TState]
} deriving (Show)

acceptableDistance :: TState
acceptableDistance = Zn

distanceBrackets :: Int
distanceBrackets = 2

evolutionize :: Int -> [Sample] -> [[Solver]] -> [[Solver]]
evolutionize 0 _ solvers = solvers
evolutionize steps samples solvers = evolutionize nSteps samples nSolvers
    where nSteps = steps - 1
          nSolvers = evolSamples samples solvers

evolSamples :: [Sample] -> [[Solver]] -> [[Solver]]
evolSamples [] solvers     = solvers
evolSamples sample solvers = undefined-- foldr (map ) solvers sample

-- computeSolver :: [Solver] -> Sample -> [Solver]
-- computeSolver [] sp@(Sample spI _) = computeSolver [initialSolver] sp
-- computeSolver srs sp@(Sample spI spO) = undefined
--     where innerMap sr sp = applySolver sr sp
--           applySrs = map (\sr -> innerMap sr sp ) srs
--           mutateSrs = map

initialSolver :: Solver
initialSolver = Solver Zn shiftTBy

applySolver :: Solver -> Sample -> InnerMap
applySolver sr@(Solver srW srF) (Sample spI spO) = InnerMap mapped distance
    where mapped = map (srF srW) spI
          distance = costFunction mapped spO

accomodateSolver :: [Solver] -> [InnerMap] -> [Solver]
accomodateSolver srs innM = undefined
    where colluded = zip srs innM

mutateSolver :: Solver -> TState -> TState -> Solver
mutateSolver sr input expected = until compareF shiftF sr
    where compareF (Solver srW srF) = expected == srF srW input
          shiftF (Solver srW srF) = Solver (shiftTP srW) srF




          -- --------------------------------
          --
          -- sampleIn = take 15 $ randoms gen :: [TState]
          -- (_, g') = next gen
          -- sampleOut = take 10 $ randoms g' :: [TState]
          -- sp@(Sample spI spO) = Sample sampleIn sampleOut
          --
          -- sr@(Solver srW srF) = initialSolver
          --
          -- srs = [initialSolver]
          -- innerMap sr sp = applySolver sr sp
          -- applySrs = map (\sr -> innerMap sr sp ) srs
          --
          --
          -- mutateSolver sr Zn Zn
          -- mutateSolver sr Pn Zn
          -- mutateSolver sr Mn Zn
          --
          -- mutateSolver sr Mn Pn
          -- mutateSolver sr Zn Pn
          -- mutateSolver sr Pn Pn
          --
          -- mutateSolver sr Zn Mn
          -- mutateSolver sr Pn Mn
          -- mutateSolver sr Mn Mn
