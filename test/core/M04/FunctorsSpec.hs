-- | Tests for "M04.Functors": each hand-written functor agrees with its @base@
-- counterpart, after converting to it. The two sides of each property run
-- different instances — ours, then the conversion, against the conversion,
-- then @base@'s — so a mistake in ours shows up as a disagreement. The
-- functions mapped are fixed; generated functions are exercise 3.
module M04.FunctorsSpec (tests) where

import Data.Foldable (toList)
import M04.Functors
import Testing.Check (Gen, arbitrary, forAll)
import Testing.Runner

tests :: [TestGroup]
tests = [result, pair, reader]

results :: Gen (Result Int Int)
results = arbitrary

pairs :: Gen (Pair Int Int)
pairs = arbitrary

result :: TestGroup
result =
  group
    "Result: agrees with Either"
    [ forAll "fmap (+ 1)" results (\r -> toEither (fmap (+ 1) r) == fmap (+ 1) (toEither r))
    , forAll "fmap show" results (\r -> toEither (fmap show r) == fmap show (toEither r))
    , forAll "toList" results (\r -> toList r == toList (toEither r))
    , forAll "length" results (\r -> length r == length (toEither r))
    , forAll "sum" results (\r -> sum r == sum (toEither r))
    , assertEqual "fmap on Value" (Value 2) (fmap (+ 1) (Value 1 :: Result String Int))
    , assertEqual "fmap on Error" (Error "e") (fmap (+ 1) (Error "e" :: Result String Int))
    , assertEqual "length of Error" 0 (length (Error "e" :: Result String Int))
    , assertEqual "length of Value" 1 (length (Value 1 :: Result String Int))
    ]

pair :: TestGroup
pair =
  group
    "Pair: agrees with (,)"
    [ forAll "fmap (+ 1)" pairs (\p -> toTuple (fmap (+ 1) p) == fmap (+ 1) (toTuple p))
    , forAll "fmap show" pairs (\p -> toTuple (fmap show p) == fmap show (toTuple p))
    , forAll "toList" pairs (\p -> toList p == toList (toTuple p))
    , forAll "length" pairs (\p -> length p == length (toTuple p))
    , forAll "sum" pairs (\p -> sum p == sum (toTuple p))
    , assertEqual "fmap keeps the first field" (Pair "x" 6) (fmap (+ 1) (Pair "x" (5 :: Int)))
    , -- Even when both fields have the same type, only the second is an element.
      assertEqual "length is 1" 1 (length (Pair (1 :: Int) (2 :: Int)))
    ]

-- | Functions have no 'Eq', so both sides are applied to a generated argument.
reader :: TestGroup
reader =
  group
    "Reader: agrees with (->) r"
    [ forAll
        "fmap (+ 1)"
        (arbitrary :: Gen Int)
        (\x -> runReader (fmap (+ 1) (Reader double)) x == fmap (+ 1) double x)
    , forAll
        "fmap show"
        (arbitrary :: Gen Int)
        (\x -> runReader (fmap show (Reader double)) x == fmap show double x)
    , assertEqual "fmap runs after the function" 11 (runReader (fmap (+ 1) (Reader double)) 5)
    ]
  where
    double :: Int -> Int
    double = (* 2)

runReader :: Reader r a -> r -> a
runReader (Reader g) = g
