{-# LANGUAGE InstanceSigs #-}

-- | Module 2, exercise 2: a number type of your own (fraction or money) with a full Num instance.
module M02.Fraction (Fraction, fraction) where

import Data.Ratio (denominator, numerator)

data Fraction = Fraction Integer Integer

fraction :: Integer -> Integer -> Maybe Fraction
fraction _ 0 = Nothing
fraction n d =
  let
    a = gcd n d
   in
    if d < 0
      then fraction (-n) (abs d)
      else Just (Fraction (n `div` a) (d `div` a))

-- | May throw an error if the denominator is zero
reduce :: Integer -> Integer -> Fraction
reduce n d =
  case fraction n d of
    Nothing -> error "Fraction has zero denominator"
    Just f -> f

-- Instances
instance Eq Fraction where
  (==) (Fraction n1 d1) (Fraction n2 d2) = n1 == n2 && d1 == d2

instance Show Fraction where
  showsPrec d (Fraction a b) =
    showParen (d > 7) $
      showsPrec 11 a
        . showString " / "
        . showsPrec 11 b

instance Ord Fraction where
  compare (Fraction n1 d1) (Fraction n2 d2) = compare (n1 * d2) (n2 * d1)

instance Num Fraction where
  (+) (Fraction n1 d1) (Fraction n2 d2) =
    reduce (n1 * d2 + n2 * d1) (d1 * d2)

  (-) (Fraction n1 d1) (Fraction n2 d2) =
    reduce (n1 * d2 - n2 * d1) (d1 * d2)

  (*) (Fraction n1 d1) (Fraction n2 d2) =
    reduce (n1 * n2) (d1 * d2)

  negate :: Fraction -> Fraction
  negate (Fraction n d) = Fraction (-n) d

  abs :: Fraction -> Fraction
  abs (Fraction n d) = Fraction (abs n) d

  signum :: Fraction -> Fraction
  signum (Fraction n _)
    | n > 0 = 1
    | n < 0 = -1
    | otherwise = 0

  fromInteger :: Integer -> Fraction
  fromInteger n = Fraction n 1

instance Fractional Fraction where
  (/) :: Fraction -> Fraction -> Fraction
  (/) f1 (Fraction n2 d2) = (*) f1 (Fraction d2 n2)

  recip :: Fraction -> Fraction
  recip (Fraction n d) = reduce d n

  fromRational :: Rational -> Fraction
  fromRational r = Fraction (numerator r) (denominator r)
