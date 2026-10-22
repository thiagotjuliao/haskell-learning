-- | Tests for "Testing.Check". Exact values depend on the generator, so most
-- cases look at properties of many draws instead: values stay in range, both
-- ends appear, and parts that should be independent do not repeat each other,
-- which is how a seed passed along twice shows up.
module Testing.CheckSpec (tests) where

import Testing.Check
import Testing.Random (Seed, mkSeed, nextInRange)
import Testing.Runner

tests :: [TestGroup]
tests =
  [ rangeTests
  , oneOfTests
  , listOfTests
  , pairTests
  , combinatorTests
  , arbitraryTests
  , resultTests
  , forAllTests
  ]

-- | One value per seed, from 500 different seeds.
draws :: Gen a -> [a]
draws g = [fst (run g (mkSeed w)) | w <- [1 .. 500]]

rangeTests :: TestGroup
rangeTests =
  group
    "Gen: range"
    [ assertEqual "inside [0, 9]" [] (filter (\x -> x < 0 || x > 9) (draws (range 0 9)))
    , assertEqual "reaches 0 and 9" (True, True) (reaches 0 9 (draws (range 0 9)))
    , assertEqual "negative bounds" (True, True) (reaches (-3) 3 (draws (range (-3) 3)))
    , assertEqual "a single value" [7] (unique (draws (range 7 7)))
    ]

oneOfTests :: TestGroup
oneOfTests =
  group
    "Gen: oneOf"
    [ assertEqual "only the given elements" [] (filter (`notElem` "abcd") picks)
    , assertEqual "every element, the first one included" "abcd" (filter (`elem` picks) "abcd")
    , assertEqual "just the first element" "x" (unique (draws (oneOf 'x' [])))
    , -- Pins the seed oneOf returns: the one its draw consumed, not a step short.
      assertEqual
        "returns the seed its draw consumed"
        [snd (nextInRange 0 3 s) | s <- seeds]
        [snd (run (oneOf 'a' "bcd") s) | s <- seeds]
    ]
  where
    picks = draws (oneOf 'a' "bcd")
    seeds = map mkSeed [1 .. 50]

listOfTests :: TestGroup
listOfTests =
  group
    "Gen: listOf"
    [ assertEqual "lengths inside [0, 20]" [] (filter (\n -> n < 0 || n > 20) lengths)
    , assertEqual "empty and full lists both appear" (True, True) (reaches 0 20 lengths)
    , assertEqual
        "elements come from the element generator"
        []
        (filter (\x -> x < 5 || x > 7) (concat (draws (listOf (range 5 7)))))
    , -- A seed reused for every element makes each list a single repeated value.
      assertEqual
        "elements of a long list are not all equal"
        []
        [xs | xs <- digitLists, length xs >= 5, all (== headOr xs) xs]
    , -- A seed reused between the length and the first element ties them together.
      assertEqual
        "the first element does not repeat the length"
        False
        (all (\xs -> headOr xs == length xs) (filter (not . null) numberLists))
    ]
  where
    lengths = map length (draws (listOf (range 0 9)))
    digitLists = draws (listOf (range 0 9))
    numberLists = draws (listOf (range 0 20))
    -- Total stand-in for head: the cases above only call it on non-empty lists.
    headOr :: [Int] -> Int
    headOr xs = case xs of
      x : _ -> x
      [] -> -1

pairTests :: TestGroup
pairTests =
  group
    "Gen: pair"
    [ assertEqual
        "runs the first generator, then the second on the seed it returns"
        [threaded s | s <- seeds]
        [run (pair (range 0 1000) (range 0 1000)) s | s <- seeds]
    , -- With 1001 values each, equal halves should be rare; a reused seed makes
      -- every pair equal.
      assertEqual
        "the two halves are independent"
        True
        (length (filter (uncurry (==)) (draws (pair (range 0 1000) (range 0 1000)))) < 5)
    ]
  where
    seeds = map mkSeed [1 .. 50]
    threaded s =
      let
        (a, s1) = run (range 0 1000) s
        (b, s2) = run (range 0 1000) s1
       in
        ((a, b), s2)

-- | Whether the draws hit the lowest and the highest value of a range.
reaches :: Int -> Int -> [Int] -> (Bool, Bool)
reaches lo hi xs = (lo `elem` xs, hi `elem` xs)

-- | The distinct values, in order of first appearance.
unique :: (Eq a) => [a] -> [a]
unique = foldr (\x acc -> x : filter (/= x) acc) []

-- | Each combinator against the seed threading it stands for, on the same seeds.
combinatorTests :: TestGroup
combinatorTests =
  group
    "Gen: constant, mapGen, andThen"
    [ assertEqual
        "constant leaves the seed alone"
        [(5, s) | s <- seeds]
        [run (constant (5 :: Int)) s | s <- seeds]
    , assertEqual
        "mapGen applies the function and keeps the seed"
        [let (x, s') = run digit s in (x * 2, s') | s <- seeds]
        [run (mapGen (* 2) digit) s | s <- seeds]
    , assertEqual
        "andThen runs the second generator on the seed the first returns"
        [let (n, s1) = run digit s in run (range 0 n) s1 | s <- seeds]
        [run (digit `andThen` range 0) s | s <- seeds]
    ]
  where
    seeds = map mkSeed [1 .. 50]
    digit = range 0 9

arbitraryTests :: TestGroup
arbitraryTests =
  group
    "Gen: Arbitrary"
    [ assertEqual "Int inside [-1000, 1000]" [] (filter (\x -> abs x > 1000) ints)
    , assertEqual "Int has both signs" (True, True) (any (< 0) ints, any (> 0) ints)
    , assertEqual
        "Integer inside [-1000, 1000]"
        []
        (filter (\x -> abs x > 1000) (draws (arbitrary :: Gen Integer)))
    , assertEqual "Bool gives both values" [False, True] (filter (`elem` bools) [False, True])
    , assertEqual
        "lists have lengths inside [0, 20]"
        []
        (filter (> 20) (map length (draws (arbitrary :: Gen [Int]))))
    , assertEqual
        "pair halves are independent"
        True
        (length (filter (uncurry (==)) (draws (arbitrary :: Gen (Int, Int)))) < 5)
    ]
  where
    ints = draws (arbitrary :: Gen Int)
    bools = draws (arbitrary :: Gen Bool)

-- | The runner functions forAll builds its result from.
resultTests :: TestGroup
resultTests =
  group
    "Runner: success, failure, hasFailed"
    [ assertEqual "a success has not failed" False (hasFailed (success "s"))
    , assertEqual "a failure has failed" True (hasFailed (failure "f" "why"))
    ]

-- | 'TestResult' has 'Show' but no 'Eq', so results are compared through 'show'.
forAllTests :: TestGroup
forAllTests =
  group
    "forAll"
    [ assertEqual
        "a true property passes"
        (show (success "true"))
        (show (forAll "true" (arbitrary :: Gen Int) (const True)))
    , assertEqual
        "a false property fails"
        True
        (hasFailed (forAll "false" (arbitrary :: Gen Int) (const False)))
    , assertEqual
        "the message names the value and the seed"
        (show (failure "is 42" "property is 42 failed for value 42 with Seed 0"))
        (show (forAll "is 42" (constant (42 :: Int)) (/= 42)))
    , -- The first failing case, recomputed by hand: its seed must regenerate it.
      assertEqual
        "reports the first counterexample and the seed that produced it"
        (show (failure "non-positive" (message firstFailure)))
        (show (forAll "non-positive" (arbitrary :: Gen Int) (<= 0)))
    , -- 449 comes only from the 100th seed and -715 only from the 101st (checked
      -- in GHCi), so these pin the number of cases to exactly 100.
      assertEqual
        "tries the 100th case"
        True
        (hasFailed (forAll "not 449" (arbitrary :: Gen Int) (/= 449)))
    , assertEqual
        "stops after 100 cases"
        False
        (hasFailed (forAll "not -715" (arbitrary :: Gen Int) (/= (-715))))
    ]
  where
    firstFailure :: (Int, Seed)
    firstFailure =
      case [(v, s) | s <- map mkSeed [0 .. 99], let v = fst (run arbitrary s), v > 0] of
        found : _ -> found
        [] -> (0, mkSeed 0)
    message :: (Int, Seed) -> String
    message (v, s) = "property non-positive failed for value " ++ show v ++ " with " ++ show s
