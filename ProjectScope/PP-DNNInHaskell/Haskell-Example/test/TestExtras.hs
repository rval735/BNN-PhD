----
---- CNN-PhD version 0.1, Copyright (C) 16/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Extra functions for UTs
module TestExtras
-- (
-- )
where

import           Data.Bool (bool)

checkFails :: [Bool] -> String
checkFails xs = show count ++ " / " ++ show (length xs)
    where count = foldl (\x y -> bool x (x + 1) y) 0 xs

printResult :: [Bool] -> IO ()
printResult xs = do
    print xs
    print $ checkFails xs
    print "------------"
