----
---- CNN-PhD version 0.1, Copyright (C) 22/Dic/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Source:
-- https://www.arcadianvisions.com/blog/2011/haskell-genetic-algorithm-hello-world.html

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module GenAlgoExample
-- (
-- someFunc
-- )
where

import           Control.Applicative     (liftA2)
import           Control.Arrow           (second)
import           Control.Monad           (liftM, replicateM)
import           Control.Monad.Random    (Rand, RandomGen, evalRandIO,
                                          getRandom, getRandomR)
import           Data.Function           (on)
import           Data.List               (minimumBy, nub, sortBy, (\\))
import           Data.Ord                (comparing)
import           Text.Printf             (printf)

import           Test.HUnit              hiding (assert)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Debug.Trace
import           System.IO.Unsafe        (unsafePerformIO)

type Gene = String

----- Genetic modifications
mate :: RandomGen g => Gene -> Gene -> Rand g Gene
mate g1 g2 = (++) <$> flip take g1 <*> flip drop g2 <$> pivot
  where pivot = getRandomR (0, length g1 - 1)

mutate :: RandomGen g => Gene -> Rand g Gene
mutate g = (uncurry (++) .) . second . (:) <$> delta <*> parts
  where delta = getRandomR (' ', 'z')
        idx = getRandomR (0, length g - 1)
        parts = second tail . flip splitAt g <$> idx

----- Evolution metric
target :: Gene
target = "Hello, world!"

fitness :: Gene -> Int
fitness = sum . map abs . zipWith ((-) `on` fromEnum) target

----- Random generation
randomGene :: RandomGen g => Rand g Gene
randomGene = replicateM (length target) $ getRandomR (' ', 'z')

randomPop :: RandomGen g => PopInfo -> Rand g Population
randomPop = liftA2 (,) <$> pure <*> flip replicateM randomGene . size

----- Population parametrization
data PopInfo = PopInfo {
    size      :: Int,
    crossover :: Float,
    elitism   :: Float,
    mutation  :: Float
} deriving (Show)

type Population = (PopInfo, [Gene])

defaultPop :: PopInfo
defaultPop = PopInfo 1024 0.8 0.1 0.03

maxGenerations :: Int
maxGenerations = 16384

tournamentSize :: Int
tournamentSize = 3

----- Selection of the best
tournamentSelection :: RandomGen g => Population -> Rand g Gene
tournamentSelection (info, genes) =
    minimumBy (comparing fitness) .  map (genes !!) <$>
    replicateM tournamentSize (getRandomR (0, size info - 1))

twoM :: Monad m => m a -> m (a, a)
twoM = liftM (\[x,y] -> (x,y)) . replicateM 2

selectParents :: RandomGen g => Population -> Rand g (Gene, Gene)
selectParents = twoM . tournamentSelection

----- Applied Evolution
evolve :: RandomGen g => Population -> Rand g Population
evolve p@(info@(PopInfo {size, crossover, elitism, mutation}), genes) =
    -- Return a population that is sorted by the "fitness" value of a list
    -- of (idx) "genes", where the elite is chosen to be mated or mutated
    (info,) . sortBy (comparing fitness) . (take idx genes ++) <$>
    replicateM (size - idx) (twoM getRandom >>= go)
    where idx = round (fromIntegral size * elitism)
          -- Taking two floating points, in case the first (r1) is less than "crossover"
          -- it selects two genes from the population given their fitness score
          -- and mates them to create a child that could be mutated corresponding
          -- to the second floating value (r2). In any other case, it will mutate
          -- an element given the r2 value
          go (r1,r2) | r1 <= crossover = selectParents p >>= uncurry mate >>= addChild r2
                     | otherwise = addMutation r2
          -- Given a floating point "r" and a gene, it will try to mutate the gene
          -- if less than "mutation", otherwise it will simply return the same value
          addChild r c
              | r <= mutation = mutate c
              | otherwise = return c
          -- Given a floating point "r", if the value is less than "mutation", it will
          -- take one from the pool of elite "genes" (idx to size) and mutate it, in
          -- case it is greater, it simply chooses one from the elite
          addMutation r
              | r <= mutation = mutate . (genes !!) =<< getRandomR (idx, size - 1)
              | otherwise = (genes !!) <$> getRandomR (idx, size - 1)

iterateUntil :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntil stop f = go
    where go x | stop x = return x
               | otherwise = f x >>= go

mainGen :: IO ()
mainGen = evalRandIO (randomPop defaultPop >>= iterateUntil done stepU . (, 0)) >>= result
    where step (p,gen) = (,) <$> evolve p <*> pure (gen+1)
          done ((_, g:_), generation) = generation == maxGenerations || fitness g == 0
          stepU (p,gen) = trace ("Gen: " ++ show gen ++ ", Population:" ++ show (bestInPop p)) (step (p, gen))

bestInPop :: Population -> Gene
bestInPop p = unsafePerformIO . evalRandIO $ tournamentSelection p

result ((_, g:_), generation)
    | generation == maxGenerations = putStrLn "Maximum generations reached without success."
    | fitness g == 0 = printf "Reached target (%d): %s\n" generation g
    | otherwise = putStrLn "Evolution is hard. Let's go shopping."

----- Test code above
testGen = run (evalRandIO randomGene) >>= assert . check
  where check g = and $ map ($ g) [ (>= 0) . fitness
                                  , (== 13) . length
                                  , all (between 32 122 . fromEnum) ]
        between l r x = l <= x && x <= r

testMut = run (evalRandIO $ randomGene >>= pairWithMutant) >>= assert . check
  where pairWithMutant = liftA2 (,) <$> pure <*> mutate
        check (g,m) = length g == length m && length (nub g \\ nub m) <= 1

testMate = run (evalRandIO $ twoM randomGene >>= pairWithChild) >>=
           assert . check
  where pairWithChild (mom,dad) = (mom,dad,) <$> mate mom dad
        check (m,d,c) = length c == 13 &&
                        (and . map (\(_,y,z) -> y == z) .
                         dropWhile (\(x,y,_) -> x == y) $ zip3 m c d)

unitTests = test [ "fitness1" ~: 0 ~=? fitness "Hello, world!"
                 , "fitness2" ~: 399 ~=? fitness "H5p&J;!l<X\7l"
                 , "fitness3" ~: 297 ~=? fitness "Vc;fx#QRP8V\\$"
                 , "fitness4" ~: 415 ~=? fitness "t\\O`E_Jx$n=NF" ]

runTests = do mapM_ (quickCheck . monadicIO) [testGen, testMut, testMate]
              runTestTT unitTests
