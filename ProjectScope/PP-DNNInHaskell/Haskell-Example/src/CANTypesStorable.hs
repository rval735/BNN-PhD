----
---- CNN-PhD version 0.1, Copyright (C) 17/August/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | File that keeps all types and CAN definitions together
module CANTypesStorable
-- (
-- )
where

import           CANExtras
import           CANTypes
import qualified Data.Array.Repa      as R
import           Data.Bool
import           Data.Vector.Storable hiding (mapM, mapM_)
import qualified Data.Vector.Storable as VS
import qualified Foreign              as F
import           Foreign.C.Types

import           Data.Word            (Word32)
import           GHC.Int

nttvuSize :: Int
nttvuSize = 32

nntvuSize :: Int
nntvuSize = 32

nntmuSize :: Int
nntmuSize = 64

instance Storable NNTMU where
    alignment _ = bool nntA nttA (nntA > nttA)
        where nttA = F.alignment (undefined :: NTT)
              nntA = F.alignment (undefined :: NNT)
    sizeOf _ = nntSize + nntSize + nntSize * nntmuSize
        where nntSize = F.sizeOf (undefined :: NTT)

    {-# INLINE peek #-}
    peek p = do
        xSize <- fromIntegral <$> F.peekElemOff q 0
        ySize <- fromIntegral <$> F.peekElemOff q 1
        b <- mapM (F.peekElemOff r) [0 .. (xSize * ySize - 1)]
        return $ createNNTMU xSize ySize b
        where nntPos = F.sizeOf (undefined :: NTT) * 2
              q = F.castPtr p :: F.Ptr NTT
              r = F.castPtr $ F.plusPtr p nntPos :: F.Ptr NNT

    {-# INLINE poke #-}
    poke p mtx = do
        F.pokeElemOff q 0 $ fromIntegral xSize
        F.pokeElemOff q 1 $ fromIntegral ySize
        mapM_ (uncurry $ F.pokeElemOff r) $ zip [0 .. xSize * ySize] (R.toList mtx)
        where nntPos = F.sizeOf (undefined :: NTT) * 2
              q = F.castPtr p :: F.Ptr NTT
              (R.Z R.:. xSize R.:. ySize) = R.extent mtx
              r = F.castPtr $ F.plusPtr p nntPos :: F.Ptr NNT

instance Storable NNTVU where
    alignment _ = bool nntA nttA (nntA > nttA)
        where nttA = F.alignment (undefined :: NTT)
              nntA = F.alignment (undefined :: NNT)
    sizeOf _ = F.sizeOf (undefined :: NTT) + F.sizeOf (undefined :: NNT) * nntvuSize

    {-# INLINE peek #-}
    peek p = do
        vSize <- fromIntegral <$> F.peekElemOff q 0
        b <- mapM (F.peekElemOff r) [0 .. vSize - 1]
        return $ createNNTVU vSize b
        where nttSize = F.sizeOf (undefined :: NTT)
              q = F.castPtr p :: F.Ptr NTT
              r = F.castPtr $ F.plusPtr p nttSize :: F.Ptr NNT

    {-# INLINE poke #-}
    poke p vec = do
        F.pokeElemOff q 0 $ fromIntegral size
        mapM_ (uncurry $ F.pokeElemOff r) $ zip [0 .. size - 1] (R.toList vec)
        where nttSize = F.sizeOf (undefined :: NTT)
              q = F.castPtr p :: F.Ptr NTT
              size = R.size $ R.extent vec
              r = F.castPtr $ F.plusPtr p nttSize :: F.Ptr NNT

instance Storable NTTVU where
    alignment _ = F.alignment (undefined :: NTT)
    sizeOf _ = F.sizeOf (undefined :: NTT) * nttvuSize
    -- sizeOf vec = F.sizeOf (undefined :: NTT) * (R.size (R.extent vec) + 1)

    {-# INLINE peek #-}
    peek p = do
        vSize <- fromIntegral <$> F.peekElemOff q 0
        b <- mapM (F.peekElemOff q) [1 .. vSize]
        return $ createNTTVU vSize 0 b
        where q = F.castPtr p :: F.Ptr NTT

    {-# INLINE poke #-}
    poke p vec = do
        F.pokeElemOff q 0 size
        mapM_ (uncurry $ F.pokeElemOff q) $ zip [1 .. size] (R.toList vec)
        where q = F.castPtr p
              size = R.size $ R.extent vec

instance Storable CANTElem where
    sizeOf _ = F.sizeOf (undefined :: NTT) + (nttvuSize + 1)
    alignment _ = F.alignment (undefined :: NTT)

    {-# INLINE peek #-}
    peek p = do
        a <- F.peekElemOff q 0
        b <- F.peek r
        return (CANTElem a b)
        where elemSize = F.sizeOf (undefined :: NTT)
              q = F.castPtr p
              r = F.castPtr $ F.plusPtr p elemSize

    {-# INLINE poke #-}
    poke p (CANTElem tChange canTElem) = do
        F.pokeElemOff q 0 tChange
        F.poke r canTElem
        where q = F.castPtr p
              elemSize = F.sizeOf (undefined :: NTT)
              r = F.castPtr $ F.plusPtr p elemSize

instance Storable TrainElem where
    sizeOf _ = F.sizeOf (undefined :: NNTVU) * 2
    alignment _ = F.alignment (undefined :: NNTVU)
    {-# INLINE peek #-}
    peek p = do
        a <- F.peekElemOff q 0
        b <- F.peekElemOff q 1
        return (TrainElem a b)
        where q = F.castPtr p :: F.Ptr NNTVU

    {-# INLINE poke #-}
    poke p (TrainElem inp oup) = do
        F.pokeElemOff q 0 inp
        F.pokeElemOff q 1 oup
        where q = F.castPtr p :: F.Ptr NNTVU

-- RefLink: https://github.com/oreqizer/minicraft/blob/master/haskell/naive1.hs#L40
-- RefLink: https://hackage.haskell.org/package/vector-0.11.0.0/docs/src/Data-Vector-Unboxed-Base.html#Unbox
