-- | Tests for "M03.Monoids": the Semigroup and Monoid laws checked with
-- 'forAll' on generated values, plus examples by hand. The laws hold for both
-- orders of composition, so only the examples pin which order 'Endo' and
-- 'Dual' use.
module M03.MonoidsSpec (tests) where

import M03.Monoids
import Testing.Check (Arbitrary (..), Gen, forAll, mapGen, pair)
import Testing.Runner

tests :: [TestGroup]
tests = [numeric, ordered, choice, boolean, dual, endo, examples, nonAssociative]

-- | @(x <> y) <> z == x <> (y <> z)@ on generated triples.
associative :: (Eq a, Show a, Semigroup a) => String -> Gen a -> TestResult
associative name g =
  forAll
    (name ++ ": associative")
    (pair (pair g g) g)
    (\((x, y), z) -> (x <> y) <> z == x <> (y <> z))

-- | @mempty <> x == x@ and @x <> mempty == x@ on generated values.
identity :: (Eq a, Show a, Monoid a) => String -> Gen a -> TestResult
identity name g = forAll (name ++ ": identity") g (\x -> mempty <> x == x && x <> mempty == x)

-- | Both monoid laws for one type.
monoidLaws :: (Eq a, Show a, Monoid a) => String -> Gen a -> [TestResult]
monoidLaws name g = [associative name g, identity name g]

numeric :: TestGroup
numeric =
  group "Sum, Product: laws" $
    monoidLaws "Sum Int" (arbitrary :: Gen (Sum Int))
      ++ monoidLaws "Sum Integer" (arbitrary :: Gen (Sum Integer))
      ++ monoidLaws "Product Int" (arbitrary :: Gen (Product Int))
      ++ monoidLaws "Product Integer" (arbitrary :: Gen (Product Integer))

-- | Min and Max are monoids only over bounded types; over Integer they are
-- semigroups, so only associativity applies.
ordered :: TestGroup
ordered =
  group "Min, Max: laws" $
    monoidLaws "Min Int" (arbitrary :: Gen (Min Int))
      ++ monoidLaws "Max Int" (arbitrary :: Gen (Max Int))
      ++ [ associative "Min Integer" (arbitrary :: Gen (Min Integer))
         , associative "Max Integer" (arbitrary :: Gen (Max Integer))
         ]

choice :: TestGroup
choice =
  group "First, Last: laws" $
    monoidLaws "First Int" (arbitrary :: Gen (First Int))
      ++ monoidLaws "Last Int" (arbitrary :: Gen (Last Int))

boolean :: TestGroup
boolean =
  group "Any, All: laws" $
    monoidLaws "Any" (arbitrary :: Gen Any) ++ monoidLaws "All" (arbitrary :: Gen All)

-- | Over lists and First, where order matters, so a Dual that forgot to flip
-- would still pass these and is caught by the examples instead.
dual :: TestGroup
dual =
  group "Dual: laws" $
    monoidLaws "Dual [Int]" (arbitrary :: Gen (Dual [Int]))
      ++ monoidLaws "Dual (First Int)" (arbitrary :: Gen (Dual (First Int)))

-- | An 'Endo' with a 'Show' instance, so 'forAll' can report it: functions
-- have no 'Show', so it shows its values on a few points. The newtype is
-- local to this module, so the instance is not an orphan.
newtype Shown = Shown (Endo Int)

instance Show Shown where
  show (Shown e) = "Endo with values " ++ show (map (apply e) points) ++ " on " ++ show points

instance Arbitrary Shown where
  arbitrary = mapGen Shown arbitrary

points :: [Int]
points = [-2 .. 2]

apply :: Endo a -> a -> a
apply (Endo f) = f

-- | Functions have no 'Eq' either, so two Endos are compared by applying both
-- to a generated argument.
endo :: TestGroup
endo =
  group
    "Endo: laws"
    [ forAll
        "Endo Int: associative"
        (pair (pair (pair arbitrary arbitrary) arbitrary) arbitrary)
        ( \(((Shown f, Shown g), Shown h), x) ->
            apply ((f <> g) <> h) x == apply (f <> (g <> h)) (x :: Int)
        )
    , forAll
        "Endo Int: identity"
        (pair arbitrary arbitrary)
        ( \(Shown f, x) ->
            apply (mempty <> f) x == apply f (x :: Int)
              && apply (f <> mempty) x == apply f x
        )
    ]

examples :: TestGroup
examples =
  group
    "Monoids: examples"
    [ assertEqual "Sum" (Sum 6) (Sum 1 <> Sum 2 <> Sum (3 :: Int))
    , assertEqual "Product" (Product 24) (Product 2 <> Product 3 <> Product (4 :: Int))
    , assertEqual "Min, mempty is maxBound" (Min (maxBound :: Int)) mempty
    , assertEqual "Max, mempty is minBound" (Max (minBound :: Int)) mempty
    , assertEqual "Min" (Min 1) (Min 3 <> Min 1 <> Min (2 :: Int))
    , assertEqual "Max" (Max 3) (Max 3 <> Max 1 <> Max (2 :: Int))
    , assertEqual
        "First skips Nothing"
        (First (Just 2))
        (First Nothing <> First (Just 2) <> First (Just (3 :: Int)))
    , assertEqual
        "Last skips Nothing"
        (Last (Just 2))
        (Last (Just 1) <> Last (Just 2) <> Last (Nothing :: Maybe Int))
    , assertEqual "Any" (Any True, Any False) (Any False <> Any True, Any False <> mempty)
    , assertEqual "All" (All False, All True) (All True <> All False, All True <> mempty)
    , assertEqual "Dual flips the order" (Dual [2, 1]) (Dual [1] <> Dual [2 :: Int])
    , assertEqual "Endo composes right to left" 11 (apply (Endo (+ 1) <> Endo (* 2)) (5 :: Int))
    , assertEqual
        "Dual (Endo) composes left to right"
        12
        (apply (undual (Dual (Endo (+ 1)) <> Dual (Endo (* 2)))) (5 :: Int))
    ]
  where
    undual :: Dual a -> a
    undual (Dual a) = a

-- | The checker has to find a counterexample for an operation that is not
-- associative, or the law tests above could be passing vacuously.
nonAssociative :: TestGroup
nonAssociative =
  group
    "Non-associative operations"
    [ assertEqual
        "subtraction"
        True
        (hasFailed (forAll "-" triples (\((x, y), z) -> (x - y) - z == x - (y - z))))
    , assertEqual
        "average of two"
        True
        (hasFailed (forAll "average" triples (\((x, y), z) -> avg (avg x y) z == avg x (avg y z))))
    ]
  where
    triples :: Gen ((Integer, Integer), Integer)
    triples = arbitrary
    avg :: Integer -> Integer -> Integer
    avg a b = (a + b) `div` 2
