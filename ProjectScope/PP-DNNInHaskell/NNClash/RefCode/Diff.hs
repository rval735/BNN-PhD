----------------------------------------------------------------------
-- |
-- Module      :  Diff
-- Copyright   :  (c) vandreev 2006
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Link        :  https://vandreev.wordpress.com/2006/12/04/non-standard-analysis-and-automatic-differentiation/
-- Stability   :  experimental
--
-- Simple AD code
----------------------------------------------------------------------

module Diff where

data Diffable a = a :+ a

funcPart :: Diffable a -> a
funcPart (x :+ x') = x

diffPart :: Diffable a -> a
diffPart (x :+ x') = x'

instance (RealFloat a) => Eq (Diffable a) where
  (x :+ x') == (y :+ y') = (x == y) && (x' == y')

instance (RealFloat a, Show a) => Show (Diffable a) where
    show (x :+ x') = show (x, x')

instance (RealFloat a) => Num (Diffable a) where
  (x :+ x') + (y :+ y') = (x + y) :+ (x' + y')
  (x :+ x') * (y :+ y') = (x * y) :+ (x' * y + y' * x)
  abs (x :+ x') = if x < 0 then (-x) :+ (-x') else x :+ x'
  negate (x :+ x') = negate x :+ negate x'
  signum (x :+ x') = signum x :+ 0
  fromInteger x = fromInteger x :+ 1

instance (RealFloat a) => Fractional (Diffable a) where
  fromRational x = (fromRational x) :+ 1
  recip (x :+ x') = (recip x) :+ (negate x' / (x^2))

instance (RealFloat a) => Floating (Diffable a) where
  pi = pi :+ 0
  exp (x :+ x') = (exp x) :+ (x' * exp x)
  log (x :+ x') = (log x) :+ (x' * recip x)
  sin (x :+ x') = (sin x) :+ (x' * cos x)
  cos (x :+ x') = (cos x) :+ (x' * (negate $ sin x))
  sinh (x :+ x') = (sinh x) :+ (x' * cosh x)
  cosh (x :+ x') = (cosh x) :+ (x' * sinh x)
  asin (x :+ x') = (asin x) :+ (x' / (sqrt $ 1-x^2))
  acos (x :+ x') = (acos x) :+ (x' / (negate (sqrt $ 1-x^2)))
  atan (x :+ x') = (atan x) :+ (x' / (x^2+1))
  asinh (x :+ x') = (asinh x) :+ (x' / (sqrt $ x^2+1))
  acosh (x :+ x') = (acosh x) :+ (x' / (negate (sqrt $ x^2-1)))
  atanh (x :+ x') = (atanh x) :+ (x' / (1 - x^2))


----------------------------------------------------------------------
-- Another example: http://augustss.blogspot.ca/2007/04/overloading-haskell-numbers-part-2.html
----------------------------------------------------------------------

data PD a = P a a deriving (Eq, Ord, Show)
instance Num a => Num (PD a) where
    P x x' + P y y' = P (x+y) (x'+y')
    P x x' - P y y' = P (x-y) (x'-y')
    P x x' * P y y' = P (x*y) (x*y' + y'*x)
    fromInteger i   = P (fromInteger i) 0
    abs (P x x') = P (abs x) (signum x * x')
    signum (P x x') = P (signum x) 0

instance Fractional a => Fractional (PD a) where
    P x x' / P y y' = P (x / y) ( (x'*y - x*y') / (y * y))
    fromRational r  = P (fromRational r) 0

data Dif a = D a (Dif a)

val (D x _) = x

df (D _ x') = x'

dVar x = D x 1

instance (Show a) => Show (Dif a) where
    show x = show (val x)

instance (Eq a) => Eq (Dif a) where
    x == y  =  val x == val y

instance (Ord a) => Ord (Dif a) where
    x `compare` y  =  val x `compare` val y

instance (Num a) => Num (Dif a) where
    D x x' + D y y'          =  D (x + y) (x' + y')
    D x x' - D y y'          =  D (x - y) (x' - y')
    p@(D x x') * q@(D y y')  =  D (x * y) (x' * q + p * y')
    fromInteger i            =  D (fromInteger i) 0
    abs p@(D x x')           =  D (abs x) (signum p * x')
    signum (D x _)           =  D (signum x) 0

instance (Fractional a) => Fractional (Dif a) where
    recip (D x x') = ip
        where ip = D (recip x) (-x' * ip * ip)
    fromRational r = D (fromRational r) 0

lift (f : f') p@(D x x') = D (f x) (x' * lift f' p)

instance (Floating a) => Floating (Dif a) where
    pi               = D pi 0
    exp (D x x')     = r where r = D (exp x) (x' * r)
    log p@(D x x')   = D (log x) (x' / p)
    sqrt (D x x')    = r where r = D (sqrt x) (x' / (2 * r))
    sin              = lift (cycle [sin, cos, negate . sin, negate . cos])
    cos              = lift (cycle [cos, negate . sin, negate . cos, sin])
    acos p@(D x x')  = D (acos x) (-x' / sqrt(1 - p*p))
    asin p@(D x x')  = D (asin x) ( x' / sqrt(1 - p*p))
    atan p@(D x x')  = D (atan x) ( x' / (p*p - 1))
    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2

-- $> import Data.Number.Symbolic
-- $> let x :: Num a => Dif (Sym a); x = dVar (var "x")
-- $> df $ x*x
-- $> df $ sin x

sqr x = convAbs $ iterate improve 1
    where improve r = (r + x/r) / 2
          convAbs (x1:x2:_) | abs (x1-x2) < 1e-10 = x2
          convAbs (_:xs)    = convAbs xs

g x = if abs (x - 0.7) < 0.4 then x else g (cos x)

findZero f = convRel $ cut $ iterate step start
    where step x = x - val fx / val (df fx) where fx = f (dVar x)
          start = 1  -- just some value
          epsilon = 1e-10
          cut = (++ error "No convergence in 1000 steps") . take 1000
          convRel (x1:x2:_) | x1 == x2 || abs (x1+x2) / abs (x1-x2) > 1/epsilon = x2
          convRel (_:xs) = convRel xs
