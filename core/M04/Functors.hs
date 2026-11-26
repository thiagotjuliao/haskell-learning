-- | Module 4, exercise 1: Either e, (,) a and (->) r by hand, as functors.
module M04.Functors (
  Result (..),
  Pair (..),
  Reader (..),
  toEither,
  toTuple,
) where

import Testing.Check (Arbitrary (..), andThen, mapGen, oneOf)

data Result e a = Error e | Value a
  deriving (Show, Eq)

toEither :: Result e a -> Either e a
toEither (Error e) = Left e
toEither (Value a) = Right a

data Pair a b = Pair a b
  deriving (Show, Eq)

toTuple :: Pair a b -> (a, b)
toTuple (Pair a b) = (a, b)

newtype Reader r a = Reader (r -> a)

instance Functor (Result e) where
  fmap _ (Error e) = Error e
  fmap f (Value a) = Value (f a)

instance Foldable (Result e) where
  foldMap _ (Error _) = mempty
  foldMap op (Value a) = op a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Result e a) where
  arbitrary =
    arbitrary
      `andThen` \(e, a) -> oneOf (Error e) [Value a]

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap op (Pair _ b) = op b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = mapGen (uncurry Pair) arbitrary

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)
