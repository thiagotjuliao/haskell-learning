{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module 3, exercise 4: newtypes whose instances come from GeneralizedNewtypeDeriving
-- and DerivingVia.
module M03.Deriving (
  SumD (..),
  ProductD (..),
  FirstD (..),
  LastD (..),
  MaxD (..),
  MinD (..),
  AnyD (..),
  AllD (..),
  DualD (..),
  EndoD (..),
) where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import M03.Monoids (Endo (..))
import Testing.Check (Arbitrary)

newtype SumD a = SumD a
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (M.Sum a)

newtype ProductD a = ProductD a
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (M.Product a)

newtype FirstD a = FirstD (Maybe a)
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (M.First a)

newtype LastD a = LastD (Maybe a)
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (M.Last a)

newtype MaxD a = MaxD a
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (S.Max a)

newtype MinD a = MinD a
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (S.Min a)

newtype AnyD = AnyD Bool
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via M.Any

newtype AllD = AllD Bool
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via M.All

newtype DualD a = DualD a
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (M.Dual a)

newtype EndoD a = EndoD (a -> a)
  deriving (Arbitrary) via (Endo a)
  deriving (Semigroup, Monoid) via (M.Endo a)
