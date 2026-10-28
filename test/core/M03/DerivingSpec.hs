-- | Tests for "M03.Deriving": each newtype derived via its @base@ counterpart
-- must agree with the hand-written one from "M03.Monoids". Manual values are
-- generated, combined with the manual '<>', and compared with the derived
-- '<>' of their converted twins; 'mempty' is compared directly.
module M03.DerivingSpec (tests) where

import M03.Deriving
import M03.Monoids
import Testing.Check (Arbitrary (..), Gen, forAll, mapGen, pair)
import Testing.Runner

tests :: [TestGroup]
tests = [numeric, ordered, choice, boolean, dual, endo]

-- | The manual '<>' and the derived '<>' agree on generated pairs.
combineAgrees ::
  (Show m, Semigroup m, Semigroup d, Eq d) => String -> (m -> d) -> Gen m -> TestResult
combineAgrees name toDerived g =
  forAll
    (name ++ ": <> agrees")
    (pair g g)
    (\(x, y) -> toDerived (x <> y) == toDerived x <> toDerived y)

-- | Both '<>' and 'mempty' agree.
agrees ::
  (Show m, Monoid m, Monoid d, Eq d, Show d) => String -> (m -> d) -> Gen m -> [TestResult]
agrees name toDerived g =
  [ combineAgrees name toDerived g
  , assertEqual (name ++ ": mempty agrees") mempty (toDerived mempty)
  ]

numeric :: TestGroup
numeric =
  group "Sum, Product: derived agrees with manual" $
    agrees "Sum Int" (\(Sum a) -> SumD a) (arbitrary :: Gen (Sum Int))
      ++ agrees "Product Int" (\(Product a) -> ProductD a) (arbitrary :: Gen (Product Int))

ordered :: TestGroup
ordered =
  group "Min, Max: derived agrees with manual" $
    agrees "Min Int" (\(Min a) -> MinD a) (arbitrary :: Gen (Min Int))
      ++ agrees "Max Int" (\(Max a) -> MaxD a) (arbitrary :: Gen (Max Int))

choice :: TestGroup
choice =
  group "First, Last: derived agrees with manual" $
    agrees "First Int" (\(First a) -> FirstD a) (arbitrary :: Gen (First Int))
      ++ agrees "Last Int" (\(Last a) -> LastD a) (arbitrary :: Gen (Last Int))

boolean :: TestGroup
boolean =
  group "Any, All: derived agrees with manual" $
    agrees "Any" (\(Any a) -> AnyD a) (arbitrary :: Gen Any)
      ++ agrees "All" (\(All a) -> AllD a) (arbitrary :: Gen All)

-- | Over lists, where the order of '<>' shows.
dual :: TestGroup
dual =
  group "Dual: derived agrees with manual" $
    agrees "Dual [Int]" (\(Dual a) -> DualD a) (arbitrary :: Gen (Dual [Int]))

-- | An 'Endo' that 'forAll' can show, by its values on a few points: functions
-- have no 'Show'. Local to this module, so the instance is not an orphan.
newtype Shown = Shown (Endo Int)

instance Show Shown where
  show (Shown (Endo f)) = "Endo with values " ++ show (map f points) ++ " on " ++ show points

instance Arbitrary Shown where
  arbitrary = mapGen Shown arbitrary

points :: [Int]
points = [-2 .. 2]

applyD :: EndoD a -> a -> a
applyD (EndoD f) = f

-- | Functions have no 'Eq', so both sides are applied to a generated argument.
endo :: TestGroup
endo =
  group
    "Endo: derived agrees with manual"
    [ forAll
        "Endo Int: <> agrees"
        (pair (pair arbitrary arbitrary) arbitrary)
        ( \((Shown (Endo f), Shown (Endo g)), x) ->
            apply (Endo f <> Endo g) x == applyD (EndoD f <> EndoD g) (x :: Int)
        )
    , forAll
        "Endo Int: mempty agrees"
        arbitrary
        (\x -> apply mempty x == applyD mempty (x :: Int))
    , assertEqual
        "Endo composes right to left in both"
        (11, 11)
        (apply (Endo (+ 1) <> Endo (* 2)) 5, applyD (EndoD (+ 1) <> EndoD (* 2)) (5 :: Int))
    ]
  where
    apply :: Endo a -> a -> a
    apply (Endo f) = f
