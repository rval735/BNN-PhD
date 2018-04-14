----
---- CNN-PhD version 0.1, Copyright (C) 9/Apr/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ExplicitForAll          #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}

module Misc
-- (
--
-- )
where

import           Data.Constraint ((:-), Dict (..))
import           GHC.Types       (Constraint)

type C1 (con :: u -> Constraint) a = con a
type C2 (con :: u -> Constraint) a b = (con a, con b)
type C3 (con :: u -> Constraint) a b c = (con a, con b, con c)
type C4 (con :: u -> Constraint) a b c d = (con a, con b, con c, con d)
type C5 (con :: u -> Constraint) a b c d e = (con a, con b, con c, con d, con e)
type C6 (con :: u -> Constraint) a b c d e f = (con a, con b, con c, con d, con e, con f)

class HasCon a where
  type Con a :: Constraint
  toDict :: a -> Dict (Con a)
  unDict :: Con a => a

newtype Sat kon a = Sat (Dict (kon a))

class    Yes1 a

instance Yes1 a

type Yes1' = Sat Yes1

infixl 7 :*
type (:*)  = (,)

infixr 3 &&
type (&&) = (:*)

infixr 1 |-
newtype a |- b = Entail (Con a :- Con b)

class OpCon op con where
    inOp :: con a && con b |- con (a `op` b)


type Prod k = (:*)


