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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
--
-- {-# LANGUAGE GADTs         #-}
-- {-# LANGUAGE Rank2Types    #-}
-- {-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module GAD
-- (
--     nnFunction
-- )
where

import           Cat
import           Data.Constraint as Constraint
import qualified Prelude         as PP

newtype GAD k a b = GAD (a -> (b, k a b))

linearD :: (a -> b) -> k a b -> GAD k a b
linearD f f' = GAD (\x -> (f x, f'))

type AD a b = GAD (->) a b

instance Category (GAD k) where
    id = PP.undefined -- linearD id id
    (.) = PP.undefined
    -- id = linearD id id
    -- (.) = \(GAD g) (GAD f) -> GAD (\x -> let {(y, f') = f x; (z, g') = g y} in (z, g' . f'))


-- instance Category k => Category (GD k) where
--     type Ok (GD k) = Ok k
--     Linear(id)
--     D g . D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' . f'))
