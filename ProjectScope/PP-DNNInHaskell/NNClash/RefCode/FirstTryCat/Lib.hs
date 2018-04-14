----
---- CNN-PhD version 0.1, Copyright (C) 6/Apr/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Lib
-- (
--     nnFunction
-- )
where


nnFunction :: IO ()
nnFunction = putStrLn "Hello!!"

class VectorSpace v s | v -> s where
    zeroV :: v
    (*^) :: s -> v -> v
    (+^) :: v -> v -> v
    negateV :: v -> v

-- class VectorSpace v s => InnerSpace v s | v -> s where
--     (<.>) :: v -> v -> s
--
-- instance VectorSpace v s => VectorSpace (a->v) s where
--     zeroV   = pure   zeroV
--     (*^) s  = fmap   (s *^)
--     (+^)   = liftA2 (+^)
--     negateV = fmap   negateV

-- type a -* b = a -> b
-- type a >< b = (a, b)

-- (∆) :: (b -> c) -> (a -> b) -> (a -> c)
-- f ∆ g = \x -> let {y = f x; z = g y} in z



-- instance Monoidal (->) where
--     f >< g = \x y -> (f x, g y)
--(><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
-- f >< g = (f a, g b)
-- infixr 7 ><

