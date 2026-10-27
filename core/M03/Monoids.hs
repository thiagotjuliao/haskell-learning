-- | Module 3, exercise 3: the classic Semigroup and Monoid newtypes, written by hand.
module M03.Monoids (
  Sum (..),
  Product (..),
  Min (..),
  Max (..),
  First (..),
  Last (..),
  Any (..),
  All (..),
  Dual (..),
  Endo (..),
) where

import Testing.Check (
  Arbitrary (..),
  mapGen,
 )

-- Sum
newtype Sum a = Sum a deriving (Eq, Show)

instance (Num a) => Semigroup (Sum a) where
  (<>) (Sum a1) (Sum a2) = Sum (a1 + a2)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

instance (Arbitrary a) => Arbitrary (Sum a) where
  arbitrary = mapGen Sum arbitrary

-- Product
newtype Product a = Product a deriving (Eq, Show)

instance (Num a) => Semigroup (Product a) where
  (<>) (Product a1) (Product a2) = Product (a1 * a2)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

instance (Arbitrary a) => Arbitrary (Product a) where
  arbitrary = mapGen Product arbitrary

-- Min
newtype Min a = Min a deriving (Eq, Show)

instance (Ord a) => Semigroup (Min a) where
  (<>) (Min a1) (Min a2) = Min (min a1 a2)

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

instance (Arbitrary a) => Arbitrary (Min a) where
  arbitrary = mapGen Min arbitrary

-- Max
newtype Max a = Max a deriving (Eq, Show)

instance (Ord a) => Semigroup (Max a) where
  (<>) (Max a1) (Max a2) = Max (max a1 a2)

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

instance (Arbitrary a) => Arbitrary (Max a) where
  arbitrary = mapGen Max arbitrary

-- First
newtype First a = First (Maybe a) deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) (First Nothing) b = b
  (<>) a _ = a

instance Monoid (First a) where
  mempty = First Nothing

instance (Arbitrary a) => Arbitrary (First a) where
  arbitrary = mapGen First arbitrary

-- Last
newtype Last a = Last (Maybe a) deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) a (Last Nothing) = a
  (<>) _ b = b

instance Monoid (Last a) where
  mempty = Last Nothing

instance (Arbitrary a) => Arbitrary (Last a) where
  arbitrary = mapGen Last arbitrary

-- Any
newtype Any = Any Bool deriving (Eq, Show)

instance Semigroup Any where
  (<>) (Any a) (Any b) = Any (a || b)

instance Monoid Any where
  mempty = Any False

instance Arbitrary Any where
  arbitrary = mapGen Any arbitrary

-- All
newtype All = All Bool deriving (Eq, Show)

instance Semigroup All where
  (<>) (All a) (All b) = All (a && b)

instance Monoid All where
  mempty = All True

instance Arbitrary All where
  arbitrary = mapGen All arbitrary

-- Dual
newtype Dual a = Dual a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Dual a) where
  (<>) (Dual a) (Dual b) = Dual (b <> a)

instance (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty

instance (Arbitrary a) => Arbitrary (Dual a) where
  arbitrary = mapGen Dual arbitrary

-- Endo
newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

instance (Num a, Arbitrary a) => Arbitrary (Endo a) where
  arbitrary = mapGen (\a -> Endo (\x -> a * x + a)) arbitrary
