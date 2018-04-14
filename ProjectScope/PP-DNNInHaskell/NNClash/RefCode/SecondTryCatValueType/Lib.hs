----
---- CNN-PhD version 0.1, Copyright (C) 6/Apr/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- {-# LANGUAGE FlexibleContexts       #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE InstanceSigs           #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE TypeOperators          #-}
-- {-# LANGUAGE UndecidableInstances   #-}

-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies  #-}


{-# LANGUAGE GADTs         #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

module Lib
-- (
--     nnFunction
-- )
where

nnFunction :: IO ()
nnFunction = putStrLn "Hello!!"


-- data Cat k where
--     IdC  :: forall a. a `k` a
--     AppC :: forall a b c. a (b `k` c) -> (a `k` b) -> (a `k` c)

-- data Maybee a where
--     Nothing :: Maybee a
--     Just :: a -> Maybee a

type (-*) = (->)
type (><) = (,)
type (<>) = Either
type Prod k = (><)

infixl 7 ><
infixl 6 <>

data Cat k = Cat {
    idC  :: forall a . a `k` a,
    appC :: forall a b c . (b `k` c) -> (a `k` b) -> (a `k` c)
    }

data (:**:) :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> * where
    (:**:) :: c1 a1 b1 -> c2 a2 b2 -> (:**:) c1 c2 (a1, a2) (b1, b2)

newtype Monoidal k = Monoidal {
    forkC :: forall a b c d . a `k` c -> b `k` d -> (a >< b) `k` (c >< d)
    }

data Cartesian k = Cartesian {
    exl :: forall a b . (a >< b) `k` a,
    exr :: forall a b . (a >< b) `k` b,
    dup :: forall a b . a `k` (a >< a)
    }

data Cocartesian k = Cocartesian {
    inl   :: forall a b . a `k` (a <> b),
    inr   :: forall a b . b `k` (a <> b),
    joinC :: forall a b c . (a `k` b) -> (b `k` c) -> (a `k` (b <> c))
    }

data CartesianClosed k = CartesianClosed {
    applyCC   :: forall a b . (a -> b, a) `k` b,
    curryCC   :: forall a b c . (a >< b `k` c) -> (a `k` b -> c),
    uncurryCC :: forall a b c . (a `k` b -> c) -> (a >< b `k` c)
    }

-- newtype GAD k a b = GAD (a -> (b >< (a `k` b)))
newtype GAD k a b = GAD { unD :: a -> (b >< (a `k` b)) }

linearD :: (a -> b) -> (a `k` b) -> GAD k a b
linearD f f' = GAD (\x -> (f x, f'))

-- linearD :: (a -> b) -> (a -* b) -> GAD (->) a b
-- linearD f f' = GAD (f, f')

catGAD :: Cat (GAD (-*))-- (GAD (->) a b)
catGAD = Cat {
    idC = linearD id id,
    appC = \(GAD g) (GAD f) -> GAD (\x -> let {(y, f') = f x; (z, g') = g y} in (z, g' . f'))
    }

gadC :: GAD (->) Int Int
gadC = GAD (\x -> (2, (x *)))

-- monoidalFunc :: Monoidal (->)
-- monoidalFunc = Monoidal {
--     forkC = \f g (x, y) -> (f x, g y)
--     }

monoidalGAD :: Monoidal (GAD (-*))
monoidalGAD = Monoidal {
    forkC = \(GAD f) (GAD g) -> GAD (\(x,y) -> let {(w, f') = f x; (z, g') = g y} in ((w, z), (f' . g')))
    }

-- cartesianFunc :: Cartesian (->)
-- cartesianFunc = Cartesian {
--     exl = fst,
--     exr = snd,
--     forkC = \f g x -> (f x, g x)
--     }
--
-- cartesianGAD :: Cartesian (GAD (-*))
-- cartesianGAD = Cartesian {
--     exl = undefined,  -- linearD exl exl,
--     exr = undefined,  -- linearD exr exr,
--     -- forkC = undefined
--     forkC = \(GAD f) (GAD g) -> GAD (\x -> let {(y, f') = f x; (z, g') = g y} in (y, (forkC f' g' z)))
--     }

-- cartesianGAD :: Cartesian (GAD k)
-- cartesianGAD = Cartesian  {
--     exl = linearD exl exl,
--     exr = undefined ,--linearD exr exr,
--     forkC = undefined
--     }

-- data MonoidI m = MonoidI {
--     _mempty  :: m,
--     _mappend :: m -> m -> m }
--
-- monoidSum :: MonoidI Int
-- monoidSum = MonoidI {
--     _mempty  = 0,
--     _mappend = (+) }
--
-- monoidProduct :: MonoidI Int
-- monoidProduct = MonoidI {
--     _mempty  = 1,
--     _mappend = (*) }
--
-- mconcat :: MonoidI a -> [a] -> a
-- mconcat i = foldr (_mappend i) (_mempty i)
--
-- sum     = mconcat monoidSum
-- product = mconcat monoidProduct
