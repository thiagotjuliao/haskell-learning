{-# LANGUAGE InstanceSigs #-}

-- | Module 2, exercise 3: a 2x2 matrix with Num, and Fibonacci by exponentiation.
module M02.Matrix (Matrix (..), fib) where

import GHC.Show (showSpace)
import Numeric.Natural

data Matrix a = Matrix a a a a deriving (Eq)

instance (Show a) => Show (Matrix a) where
  showsPrec d (Matrix a1 a2 a3 a4) =
    showParen (d > 10) $
      showString "Matrix "
        . showsPrec 11 a1
        . showSpace
        . showsPrec 11 a2
        . showSpace
        . showsPrec 11 a3
        . showSpace
        . showsPrec 11 a4

instance (Num a) => Num (Matrix a) where
  (+) :: Matrix a -> Matrix a -> Matrix a
  (+) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    Matrix
      (a1 + b1)
      (a2 + b2)
      (a3 + b3)
      (a4 + b4)

  (*) :: Matrix a -> Matrix a -> Matrix a
  (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    Matrix
      (a1 * b1 + a2 * b3)
      (a1 * b2 + a2 * b4)
      (a3 * b1 + a4 * b3)
      (a3 * b2 + a4 * b4)

  -- Matrices do not have abs and signum definitions
  abs :: Matrix a -> Matrix a
  abs _ = error "abs cannot be applied on matrices"

  signum :: Matrix a -> Matrix a
  signum _ = error "signum cannot be applied on matrices"

  fromInteger :: Integer -> Matrix a
  fromInteger n = Matrix a 0 0 a
    where
      a = fromInteger n

  (-) :: Matrix a -> Matrix a -> Matrix a
  (-) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    Matrix
      (a1 - b1)
      (a2 - b2)
      (a3 - b3)
      (a4 - b4)

fib :: Natural -> Integer
fib n = v
  where
    (Matrix _ v _ _) = (Matrix 1 1 1 0) ^ n
