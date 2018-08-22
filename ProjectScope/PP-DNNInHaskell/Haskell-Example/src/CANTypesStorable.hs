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
import           Data.Vector.Storable hiding (length, mapM, mapM_)
import qualified Data.Vector.Storable as VS
import qualified Foreign              as F
import           Foreign.C.Types

import           Data.Word            (Word32)
import           GHC.Int

import           Unsafe.Coerce

-- Define a 4 element vector type
data Vec4 = Vec4 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat
                 {-# UNPACK #-} !CFloat deriving (Show)

-- data CANTElem = CANTElem {
--     tChange  :: NTT,
--     canTElem :: NTTVU
-- } deriving (Eq)

instance Storable VShape where
    alignment _ = F.alignment (undefined :: Int)
    sizeOf _ = F.sizeOf (undefined :: Int)
    {-# INLINE peek #-}
    peek p = R.ix1 <$> F.peek q
        where q = F.castPtr p :: F.Ptr Int
    {-# INLINE poke #-}
    poke p (R.Z R.:. x) = F.poke q x
        where q = F.castPtr p :: F.Ptr Int

instance Storable R.U where
    alignment _ = F.alignment (undefined :: Bool)
    sizeOf _ = F.sizeOf (undefined :: Bool)
    {-# INLINE peek #-}
    peek p = return $ unsafeCoerce True
    {-# INLINE poke #-}
    poke p _ = F.poke q True
        where q = F.castPtr p :: F.Ptr Bool

instance Storable NTTVU where
    alignment _ = F.alignment (undefined :: NTT)
    sizeOf vec = F.sizeOf (undefined :: NTT) * (R.size (R.extent vec) + 1)

    {-# INLINE peek #-}
    peek p = do
        print "peeked"
        vSize <- fromIntegral <$> F.peekElemOff q 0
        b <- mapM (F.peekElemOff q) [1 .. vSize]
        return $ createThreshold vSize 0 b
        where q = F.castPtr p :: F.Ptr NTT

    {-# INLINE peekElemOff #-}
    peekElemOff p idx = do
        print "peekElemOff"
        vSize <- fromIntegral <$> F.peekElemOff q 0
        b <- mapM (F.peekElemOff q) [idx + 1 .. vSize]
        return $ createThreshold (length b) 0 b
        where q = F.castPtr p :: F.Ptr NTT

    -- {-# INLINE peekByteOff #-}
    -- peekByteOff p idx = F.peekByteOff q idx8
    --     where q = F.castPtr p :: F.Ptr NTT
    --           idx8 = div idx $ F.sizeOf (undefined :: NTT)

    {-# INLINE poke #-}
    poke p vec = do
        print "poked"
        F.pokeElemOff q 0 size
        mapM_ (uncurry $ F.pokeElemOff q) $ zip [1 .. size] (R.toList vec)
        where q = F.castPtr p
              size = R.size $ R.extent vec

    {-# INLINE pokeElemOff #-}
    pokeElemOff p idx vec = do
        print "pokeElemOff"
        sizeP <- F.peekElemOff q 0
        let indexes = let sizeI = fromIntegral sizeP in bool [] [idx .. sizeI - vecSize] (sizeI > vecSize)
        mapM_ (uncurry $ F.pokeElemOff q) $ zip indexes (R.toList vec)
        where q = F.castPtr p :: F.Ptr NTT
              vecSize = R.size (R.extent vec) - 1

    -- {-# INLINE pokeByteOff #-}
    -- pokeByteOff p idx vec = do
    --
    --     F.pokeByteOff q idx8 val
    --     where q = F.castPtr p :: F.Ptr NTT
    --           idx8 = idx * F.sizeOf (undefined :: NTT)
    --           sizeV


instance Storable CANTElem where
    sizeOf (CANTElem tChange canTElem) = F.sizeOf (undefined :: NTT) + F.sizeOf canTElem
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


instance Storable Vec4 where
    sizeOf _ = F.sizeOf (undefined :: CFloat) * 4
    alignment _ = F.alignment (undefined :: CFloat)

    {-# INLINE peek #-}
    peek p = do
        a <- F.peekElemOff q 0
        b <- F.peekElemOff q 1
        c <- F.peekElemOff q 2
        d <- F.peekElemOff q 3
        return (Vec4 a b c d)
        where q = F.castPtr p

    {-# INLINE poke #-}
    poke p (Vec4 a b c d) = do
        F.pokeElemOff q 0 a
        F.pokeElemOff q 1 b
        F.pokeElemOff q 2 c
        F.pokeElemOff q 3 d
        where q = F.castPtr p

add :: Vec4 -> Vec4 -> Vec4
{-# INLINE add #-}
add (Vec4 a b c d) (Vec4 a' b' c' d') = Vec4 (a+a') (b+b') (c+c') (d+d')

mult :: Vec4 -> Vec4 -> Vec4
{-# INLINE mult #-}
mult (Vec4 a b c d) (Vec4 a' b' c' d') = Vec4 (a*a') (b*b') (c*c') (d*d')

vsum :: Vec4 -> CFloat
{-# INLINE vsum #-}
vsum (Vec4 a b c d) = a+b+c+d

a = Vec4 0.2 0.1 0.6 1.0
m = Vec4 0.99 0.7 0.8 0.6

repCount, arraySize :: Int
repCount = 10000
arraySize = 20000


data Atoms = I {-# UNPACK #-} !GHC.Int.Int32 | S {-# UNPACK #-} !GHC.Int.Int16
    deriving (Show)

instance Storable Atoms where
  sizeOf _ = 2 * F.sizeOf (undefined :: Int32)
  alignment _ = 4

  {-# INLINE peek #-}
  peek p = do
            let p1 = (F.castPtr p:: F.Ptr Word32) `F.plusPtr` 1
            t <- F.peek (F.castPtr p:: F.Ptr Word32)
            case t of
              0 -> do
                    x <- F.peekElemOff (F.castPtr p1 :: F.Ptr GHC.Int.Int32) 0
                    return (I x)
              _ -> do
                    x <- F.peekElemOff (F.castPtr p1 :: F.Ptr GHC.Int.Int16) 0
                    return (S x)

  {-# INLINE poke #-}
  poke p x = case x of
      I a -> do
              F.poke (F.castPtr p :: F.Ptr Word32) 0
              F.pokeElemOff (F.castPtr p1) 0 a
      S a -> do
              F.poke (F.castPtr p :: F.Ptr Word32) 1
              F.pokeElemOff (F.castPtr p1) 0 a
      where  p1 = (F.castPtr p :: F.Ptr Word32) `F.plusPtr` 1

kk = I 4
lsk = S 4
