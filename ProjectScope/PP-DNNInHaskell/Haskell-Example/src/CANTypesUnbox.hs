----
---- CNN-PhD version 0.1, Copyright (C) 21/August/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | File that keeps all types and CAN definitions together
module CANTypesUnbox
-- (
-- )
where

import           CANExtras
import           CANTypes
import qualified Data.Array.Repa             as R
import           Data.Bool

-- import           Control.Monad
import qualified Data.Vector                 as VB
import qualified Data.Vector.Generic.Base    as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed.Base    (Unbox)
import           GHC.Generics                (Generic)

data instance V.MVector s NTTVU = MV_NTTVU !Int !(V.MVector s NTTVU)
data instance V.Vector    NTTVU = V_NTTVU  !Int !(V.Vector    NTTVU)

instance Unbox NTTVU

instance GM.MVector V.MVector NTTVU where
    basicLength (MV_NTTVU n _) = n
    basicUnsafeSlice m n (MV_NTTVU _ vec) = MV_NTTVU n (GM.basicUnsafeSlice m n vec)
    basicOverlaps (MV_NTTVU _ vec1) (MV_NTTVU _ vec2) = GM.basicOverlaps vec1 vec2
    basicUnsafeNew n = fmap (MV_NTTVU n) (GM.basicUnsafeNew n)
    basicUnsafeRead (MV_NTTVU _ vec) = GM.basicUnsafeRead vec
    basicUnsafeWrite vec i elemR = return ()
    basicInitialize _ = return ()

instance G.Vector V.Vector NTTVU where
    basicUnsafeFreeze (MV_NTTVU n v) = fmap (V_NTTVU n) (G.basicUnsafeFreeze v)
    basicUnsafeThaw   (V_NTTVU n v) = fmap (MV_NTTVU n) (G.basicUnsafeThaw   v)
    basicLength       (V_NTTVU n _) = n
    basicUnsafeSlice m n (V_NTTVU _ v) = V_NTTVU n (G.basicUnsafeSlice (3*m) (3*n) v)
    basicUnsafeIndexM (V_NTTVU _ v) = G.basicUnsafeIndexM v

data Vectory = Vectory !Float !Float !Float deriving (Eq, Generic, Show)

data instance V.MVector s Vectory = MV_Vector !Int !(V.MVector s Float)
data instance V.Vector    Vectory =  V_Vector !Int !(V.Vector    Float)

instance Unbox Vectory

instance GM.MVector V.MVector Vectory where
    basicLength (MV_Vector n _) = n
    basicUnsafeSlice m n (MV_Vector _ v) = MV_Vector n (GM.basicUnsafeSlice (3*m) (3*n) v)
    basicOverlaps (MV_Vector _ v) (MV_Vector _ u) = GM.basicOverlaps v u
    basicUnsafeNew n = fmap (MV_Vector n) (GM.basicUnsafeNew (3*n))
    basicUnsafeRead (MV_Vector _ v) i = do
        let o = 3*i
        x <- GM.basicUnsafeRead v o
        y <- GM.basicUnsafeRead v (o+1)
        z <- GM.basicUnsafeRead v (o+2)
        return (Vectory x y z)
    basicUnsafeWrite (MV_Vector _ v) i (Vectory x y z) = do
        let o = 3*i
        GM.basicUnsafeWrite v o     x
        GM.basicUnsafeWrite v (o+1) y
        GM.basicUnsafeWrite v (o+2) z
    basicInitialize (MV_Vector _ v) = GM.basicInitialize v

instance G.Vector V.Vector Vectory where
    basicUnsafeFreeze (MV_Vector n v) = fmap (V_Vector n) (G.basicUnsafeFreeze v)
    basicUnsafeThaw   (V_Vector n v) = fmap (MV_Vector n) (G.basicUnsafeThaw   v)
    basicLength       (V_Vector n _) = n
    basicUnsafeSlice m n (V_Vector _ v) = V_Vector n (G.basicUnsafeSlice (3*m) (3*n) v)
    basicUnsafeIndexM (V_Vector _ v) i = do
        let o = 3*i
        x <- G.basicUnsafeIndexM v o
        y <- G.basicUnsafeIndexM v (o+1)
        z <- G.basicUnsafeIndexM v (o+2)
        return (Vectory x y z)

ks = Vectory 1 2 3
