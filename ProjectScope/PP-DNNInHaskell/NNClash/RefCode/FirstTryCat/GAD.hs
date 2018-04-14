----
---- CNN-PhD version 0.1, Copyright (C) 9/Apr/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module GAD
-- (
--     nnFunction
-- )
where

import           Cat
import           Misc
-----------------------------
---- Auto Diff Definition ---
-----------------------------

-- newtype D k a b = D (a -> (b, (a `k` b)))
-- linearD :: (a -> b) -> (a `k` b) -> D k a b
-- linearD f f' = D (\x -> (f x, f')

-- newtype D a b = D (a -> (b, a -> b))
--
-- linearD :: (a -> b) -> (a -> b) -> D a b
-- linearD f f' = D (\x -> (f x, f'))

-- instance Monoidal D where
--     D f >< D g = D (\x y -> let {(w, f') = f x; (z, g') = g y} in ((w, z), f' >< g'))

-- instance Categorry D where
--     idC = linearD id
--     D g • D f = D (\x -> let {(y, f') = f x; (z, g') = g y} in (z, g' • f'))

-- instance Categorry D => Cartesian D where
--     exl = linearD exl
--     exr = linearD exr
--     D f ∆ D g = D (\x -> let {(y, f') = f x; (z, g') = g y} in ((y, z), f' ∆ g'))

-- instance NumCat D where
--     negate = linearD negate
--     add = linearD add
--     mul = D (mul ∆ \a b da db -> b * da + a * db)


-- DP :: (a -> b) -> (a -> b >< (a -• b))
-- DP f a = (f a, D f a)
--
-- DU :: (a -> b) -> D a b
-- DU f = D ()


-- data GD k a b = D { unD :: a -> (b :* (a `k` b)) }
newtype GD k a b = D { unD :: a -> (b :* (a `k` b)) }

linearD :: (a -> b) -> (a `k` b) -> GD k a b
linearD f f' = D (\ a -> (f a, f'))
{-# INLINE linearD #-}

instance Category k => Category (GD k) where
    type Ok (GD k) = Ok k
    idC = linearD idC idC
    {-# INLINE (|.|) #-}
    D g |.| D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' |.| f'))


instance MonoidalPCat k => MonoidalPCat (GD k) where
      D f *** D g = D (\(a,b) -> let { (c, f') = f a; (d, g') = g b} in ((c, d), f' *** g'))
      {-# INLINE (***) #-}

instance ProductCat k => ProductCat (GD k) where
    exl = linearD exl exl
    exr = linearD exr exr
    -- dup = linearD exl


