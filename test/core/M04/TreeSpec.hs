{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Tests for "M04.Tree". 'length', 'sum', 'elem', 'maximum' and 'toList' all
-- come from 'foldMap', so checking them only against each other would pass
-- for a 'foldMap' that dropped or repeated a subtree. 'size' counts nodes by
-- structural recursion instead, and the examples pin the traversal order.
module M04.TreeSpec (tests) where

import Data.Foldable (toList)
import M04.Tree
import Testing.Check (Gen, arbitrary, forAll, pair)
import Testing.Runner

data TreeD a = LeafD | NodeD (TreeD a) a (TreeD a)
  deriving (Eq, Show, Functor, Foldable)

toTreeD :: Tree a -> TreeD a
toTreeD Leaf = LeafD
toTreeD (Node l a r) = NodeD (toTreeD l) a (toTreeD r)

data RoseD a = RoseD a [RoseD a]
  deriving (Eq, Show, Functor, Foldable)

toRoseD :: Rose a -> RoseD a
toRoseD (Rose a children) = RoseD a (toRoseD <$> children)

tests :: [TestGroup]
tests = [treeExamples, treeProperties, roseExamples, roseProperties, treeDerived, roseDerived]

trees :: Gen (Tree Int)
trees = arbitrary

roses :: Gen (Rose Int)
roses = arbitrary

-- | Nodes counted without 'Foldable'.
treeSize :: Tree a -> Int
treeSize Leaf = 0
treeSize (Node l _ r) = treeSize l + 1 + treeSize r

roseSize :: Rose a -> Int
roseSize (Rose _ children) = 1 + sum (map roseSize children)

-- | The tree from the notes, section 5.
small :: Tree Int
small = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- | Lopsided, so in order differs from pre-order and from post-order.
lopsided :: Tree Int
lopsided = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 (Node Leaf 5 Leaf))

treeExamples :: TestGroup
treeExamples =
  group
    "Tree: examples"
    [ assertEqual "toList is in order" [1, 2, 3] (toList small)
    , assertEqual "toList of a lopsided tree is in order" [1, 2, 3, 4, 5] (toList lopsided)
    , assertEqual "toList of Leaf" [] (toList (Leaf :: Tree Int))
    , assertEqual "length of Leaf" 0 (length (Leaf :: Tree Int))
    , assertEqual "sum" 15 (sum lopsided)
    , assertEqual "maximum" 5 (maximum lopsided)
    , assertEqual "elem finds a deep value" True (elem 5 lopsided)
    , assertEqual "elem of a missing value" False (elem 6 lopsided)
    , assertEqual
        "fmap keeps the shape"
        (Node (Node Leaf 10 Leaf) 20 (Node Leaf 30 Leaf))
        (fmap (* 10) small)
    ]

treeProperties :: TestGroup
treeProperties =
  group
    "Tree: Foldable agrees with toList"
    [ forAll "length counts every node" trees (\t -> length t == treeSize t)
    , forAll "length" trees (\t -> length t == length (toList t))
    , forAll "sum" trees (\t -> sum t == sum (toList t))
    , forAll "elem" (pair arbitrary trees) (\(x, t) -> elem x t == elem x (toList t))
    , -- maximum is partial: on an empty tree it throws, like on [].
      forAll "maximum" trees (\t -> null t || maximum t == maximum (toList t))
    , forAll "fmap maps toList" trees (\t -> toList (fmap (+ 1) t) == map (+ 1) (toList t))
    , forAll "fmap keeps the size" trees (\t -> treeSize (fmap show t) == treeSize t)
    ]

-- | Children in order, the second with a child of its own.
family :: Rose Int
family = Rose 1 [Rose 2 [Rose 3 []], Rose 4 []]

roseExamples :: TestGroup
roseExamples =
  group
    "Rose: examples"
    [ assertEqual "toList is pre-order" [1, 2, 3, 4] (toList family)
    , assertEqual "a single node" [7] (toList (Rose (7 :: Int) []))
    , assertEqual "length" 4 (length family)
    , assertEqual "sum" 10 (sum family)
    , assertEqual "maximum" 4 (maximum family)
    , assertEqual "elem finds a grandchild" True (elem 3 family)
    , assertEqual
        "fmap keeps the shape"
        (Rose 10 [Rose 20 [Rose 30 []], Rose 40 []])
        (fmap (* 10) family)
    ]

roseProperties :: TestGroup
roseProperties =
  group
    "Rose: Foldable agrees with toList"
    [ forAll "length counts every node" roses (\r -> length r == roseSize r)
    , forAll "length" roses (\r -> length r == length (toList r))
    , forAll "sum" roses (\r -> sum r == sum (toList r))
    , forAll "elem" (pair arbitrary roses) (\(x, r) -> elem x r == elem x (toList r))
    , -- A rose tree always has a root, so maximum is total here.
      forAll "maximum" roses (\r -> maximum r == maximum (toList r))
    , forAll "fmap maps toList" roses (\r -> toList (fmap (+ 1) r) == map (+ 1) (toList r))
    , forAll "fmap keeps the size" roses (\r -> roseSize (fmap show r) == roseSize r)
    ]

-- | DeriveFoldable folds the fields left to right, so Node l a r is in order
-- and Rose a children is pre-order: the same orders as the hand-written ones.
treeDerived :: TestGroup
treeDerived =
  group
    "Tree: derived agrees with manual"
    [ forAll "toList" trees (\t -> toList t == toList (toTreeD t))
    , forAll "fmap" trees (\t -> toTreeD (fmap (+ 1) t) == fmap (+ 1) (toTreeD t))
    , forAll "fmap show" trees (\t -> toTreeD (fmap show t) == fmap show (toTreeD t))
    ]

roseDerived :: TestGroup
roseDerived =
  group
    "Rose: derived agrees with manual"
    [ forAll "toList" roses (\r -> toList r == toList (toRoseD r))
    , forAll "fmap" roses (\r -> toRoseD (fmap (+ 1) r) == fmap (+ 1) (toRoseD r))
    , forAll "fmap show" roses (\r -> toRoseD (fmap show r) == fmap show (toRoseD r))
    ]
