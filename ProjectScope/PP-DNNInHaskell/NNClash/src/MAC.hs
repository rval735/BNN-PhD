----
---- CNN-PhD version 0.1, Copyright (C) 6/Apr/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MAC

where

import           Clash.Explicit.Testbench
import           Clash.Prelude
import           Control.Monad.State

ma acc (x,y) = acc + x * y

macT acc (x, y) = (acc', o)
    where acc' = ma acc (x, y)
          o = acc

mac = mealy macT 0

macN (x,y) = acc
    where acc = register 0 (acc + x * y)

macA (x,y) = acc
    where acc  = register 0 acc'
          acc' = ma <$> acc <*> bundle (x,y)

macST (x,y) = do
    acc <- get
    put (acc + x * y)
    return acc

asStateM :: HiddenClockReset domain gated synchronous
         => (i -> State s o)
         -> s
         -> (Signal domain i -> Signal domain o)
asStateM f i = mealy g i
    where g s x = let (o,s') = runState (f x) s
                  in  (s',o)

macS = asStateM macST 0

topEntity :: Clock System Source
          -> Reset System Asynchronous
          -> Signal System (Signed 9, Signed 9)
          -> Signal System (Signed 9)
topEntity = exposeClockReset mac
-- topEntity = exposeClockReset asStateM
-- topEntity = exposeClockReset macN
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput    = stimuliGenerator clk rst $(listToVecTH [(1,1) :: (Signed 9,Signed 9),(2,2),(3,3),(4,4)])
    expectOutput = outputVerifier clk rst $(listToVecTH [0 :: Signed 9,1,5,14])
    done         = expectOutput (topEntity clk rst testInput)
    clk          = tbSystemClockGen (not <$> done)
    rst          = systemResetGen
