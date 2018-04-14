----
---- CNN-PhD version 0.1, Copyright (C) 9/Apr/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----


{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- {-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE InstanceSigs          #-}
-- {-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
-- {-# LANGUAGE UndecidableInstances  #-}

module Cat
-- (
--     nnFunction
-- )
where

import qualified Control.Arrow as A
import           GHC.Types     (Constraint)
import           Misc

-- -----------------------------
-- ---- Cetegory Definition ----
-- -----------------------------
-- class Cat k where
--     idC :: a `k` a
--     (•) :: (b `k` c) -> (a `k` b) -> (a `k` c)
--
-- -- class Cat k => Cartesian k where
-- --     exl :: a `k` a
-- --     exr :: a `k` a
-- --     (∆) :: (b `k` c) -> (a `k` b) -> (a `k` c)
--
-- instance Cat (->) where
--     idC = id
--     g • f = g . f
--
-- class Cat k => Monoidal k where
--     prod :: (a `k` c) -> (b `k` d) -> (prod a b `k` prod c d)
--
-- -- instance Monoidal (->) where
-- --     f >< g = \x -> (f x, g x)
--
-- class Monoidal k => CartesianT k where
--     exl :: (prod a b) `k` a
--     exr :: (prod a b) `k` b
--     dup :: a `k` (prod a b)
--
-- instance CartesianT (->) where
--     exl a b = a
--     exr = \a b -> b
--     dup a = (a, a)



type Ok' k = Sat (Ok k)

type Ok2 k a b         = C2 (Ok k) a b
type Ok3 k a b c       = C3 (Ok k) a b c
type Ok4 k a b c d     = C4 (Ok k) a b c d
type Ok5 k a b c d e   = C5 (Ok k) a b c d e
type Ok6 k a b c d e f = C6 (Ok k) a b c d e f


class Category k where
    type Ok k :: * -> Constraint
    type Ok k = Yes1
    idC  :: Ok k a => a `k` a
    infixr 9 |.|
    (|.|) :: forall b c a. Ok3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

instance Category (->) where
    idC  = id
    (|.|) = (.)


------------ Product --------------

type OkProd k = OpCon (Prod k) (Ok' k)

okProd :: forall k a b. OkProd k
       => Ok' k a && Ok' k b |- Ok' k (Prod k a b)
okProd = inOp
{-# INLINE okProd #-}

class (OkProd k, Category k) => MonoidalPCat k where
    (**>) :: forall a b c d. Ok4 k a b c d => (a `k` c) -> (b `k` d) -> (Prod k a b `k` Prod k c d)
    -- default (**>) :: forall a b c d. (ProductCat k, Ok4 k a b c d) => (a `k` c) -> (b `k` d) -> (Prod k a b `k` Prod k c d)
    -- f **> g = f . exl &&& g . exr <+ okProd @k @a @b
    -- {-# INLINE (***) #-}

instance MonoidalPCat (->) where
    (**>) = (A.***)

class (OkProd k, MonoidalPCat k) => ProductCat k where
    exl :: Ok2 k a b => Prod k a b `k` a
    exr :: Ok2 k a b => Prod k a b `k` b

-- instance ProductCat (->) where
--     exl     = fst
--     exr     = snd
