----
---- CNN-PhD version 0.1, Copyright (C) 6/Apr/2018
---- Modifications: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- Original Code and Ideas are from:
-- http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Differentiation/

module AutoDiff
-- (
--
-- )
where

data D a = D a a
    deriving Show

liftD :: Num a => a -> D a
liftD x = D x 0

infinitesimal :: Num a => D a
infinitesimal = D 0 1

instance Eq a => Eq (D a) where
    D x _ == D y _ = x == y

instance Ord a => Ord (D a) where
    compare (D x _) (D y _) = compare x y

instance Num a => Num (D a) where
    D x x' + D y y' = D (x + y) (x' + y')
    D x x' * D y y' = D (x * y) (x' * y + x * y')
    negate (D x x') = D (negate x) (negate x')
    abs    (D x x') = D (abs x) (signum x * x')
    signum (D x _)  = liftD (signum x)
    fromInteger x   = liftD (fromInteger x)

instance Fractional a => Fractional (D a) where
    recip (D x x') = D (recip x) (-x'/x/x)
    fromRational x = liftD (fromRational x)
