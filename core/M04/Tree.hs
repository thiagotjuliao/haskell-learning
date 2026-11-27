-- | Module 4, exercise 2: a binary tree and a rose tree, as functors and foldables.
module M04.Tree (
  Tree (..),
  Rose (..),
) where

import Testing.Check (
  Arbitrary (..),
  Gen,
  andThen,
  constant,
  listUpTo,
  mapGen,
  range,
 )

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

data Rose a = Rose a [Rose a]
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node l a r) = (foldMap f l) <> f a <> (foldMap f r)

halt :: Gen Bool
halt = mapGen (== 3) (range 0 3)

tree :: (Arbitrary a) => (Int) -> Gen (Tree a)
tree 0 = constant Leaf
tree n =
  halt
    `andThen` \stop ->
      if stop
        then tree 0
        else
          arbitrary `andThen` \a ->
            (tree (n - 1)) `andThen` \t1 ->
              (tree (n - 1)) `andThen` \t2 ->
                constant $ Node t1 a t2

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = tree 5

instance Functor Rose where
  fmap f (Rose a children) = Rose (f a) ((fmap f) <$> children)

instance Foldable Rose where
  foldMap f (Rose a children) =
    f a <> mconcat (foldMap f <$> children)

rose :: (Arbitrary a) => Int -> Gen (Rose a)
rose 0 = arbitrary `andThen` \a -> constant $ Rose a []
rose n =
  arbitrary `andThen` \a ->
    listUpTo 5 (rose (n - 1))
      `andThen` \children ->
        constant $ Rose a children

instance (Arbitrary a) => Arbitrary (Rose a) where
  arbitrary = rose 3
