-- | Tests for "M01.ListPrelude".
--
-- Each function is checked on edge cases (empty list, single element),
-- against the real Prelude (imported qualified as @P@), and, where the
-- function should cope with it, on an infinite input.
module M01.ListPreludeSpec (tests) where

import M01.ListPrelude
import Testing.Runner
import Prelude hiding (dropWhile, filter, foldl, foldr, map, span, takeWhile, words, zipWith)
import qualified Prelude as P

tests :: [TestGroup]
tests =
  [ filterTests
  , mapTests
  , zipWithTests
  , foldlTests
  , foldrTests
  , takeWhileTests
  , takeWhile2Tests
  , dropWhileTests
  , spanTests
  , wordsTests
  ]

-- | Shared sample input: mixes evens and odds, with the predicate
-- flipping in the middle of the list.
xs :: [Int]
xs = [2, 4, 5, 6, 1, 8]

filterTests :: TestGroup
filterTests =
  group
    "filter"
    [ assertEqual "empty" [] (filter even ([] :: [Int]))
    , assertEqual "single kept" [2] (filter even [2 :: Int])
    , assertEqual "single dropped" [] (filter even [1 :: Int])
    , assertEqual "none match" [] (filter (> 100) xs)
    , assertEqual "vs Prelude" (P.filter even xs) (filter even xs)
    , assertEqual "infinite" [2, 4, 6] (take 3 (filter even [1 :: Int ..]))
    ]

mapTests :: TestGroup
mapTests =
  group
    "map"
    [ assertEqual "empty" [] (map show ([] :: [Int]))
    , assertEqual "single" ["1"] (map show [1 :: Int])
    , assertEqual "vs Prelude" (P.map (* 2) xs) (map (* 2) xs)
    , assertEqual "infinite" [2, 4, 6] (take 3 (map (* 2) [1 :: Int ..]))
    ]

zipWithTests :: TestGroup
zipWithTests =
  group
    "zipWith"
    [ assertEqual "both empty" [] (zipWith (+) [] ([] :: [Int]))
    , assertEqual "left empty" [] (zipWith (+) [] [1 :: Int])
    , assertEqual "right empty" [] (zipWith (+) [1 :: Int] [])
    , assertEqual "single" [3] (zipWith (+) [1] [2 :: Int])
    , assertEqual "left shorter" [4] (zipWith (+) [1] [3, 4 :: Int])
    , assertEqual "right shorter" [4] (zipWith (+) [1, 2] [3 :: Int])
    , assertEqual "vs Prelude" (P.zipWith (,) xs "abc") (zipWith (,) xs "abc")
    , assertEqual "one infinite" [11, 22] (zipWith (+) [1 ..] [10, 20 :: Int])
    , assertEqual "both infinite" [2, 4, 6] (take 3 (zipWith (+) [1 ..] [1 :: Int ..]))
    ]

foldlTests :: TestGroup
foldlTests =
  group
    "foldl"
    [ assertEqual "empty returns seed" 0 (foldl (+) 0 ([] :: [Int]))
    , assertEqual "single" 5 (foldl (+) 0 [5 :: Int])
    , assertEqual "sum" 55 (foldl (+) 0 [1 .. 10 :: Int])
    , assertEqual "reverses with flip (:)" "cba" (foldl (flip (:)) [] "abc")
    , assertEqual "left-associative" (((0 - 1) - 2) - 3) (foldl (-) 0 [1, 2, 3 :: Int])
    , assertEqual "vs Prelude" (P.foldl (-) 100 xs) (foldl (-) 100 xs)
    ]

foldrTests :: TestGroup
foldrTests =
  group
    "foldr"
    [ assertEqual "empty returns seed" 0 (foldr (+) 0 ([] :: [Int]))
    , assertEqual "single" 5 (foldr (+) 0 [5 :: Int])
    , assertEqual "rebuilds list with (:)" "abc" (foldr (:) [] "abc")
    , assertEqual "right-associative" (1 - (2 - (3 - 0))) (foldr (-) 0 [1, 2, 3 :: Int])
    , assertEqual "vs Prelude" (P.foldr (-) 100 xs) (foldr (-) 100 xs)
    , -- Short-circuits: (||) never looks at the rest once it sees True.
      assertEqual "infinite short-circuit" True (foldr (\x acc -> x > 10 || acc) False [1 :: Int ..])
    , assertEqual "infinite lazy map" [2, 4, 6] (take 3 (foldr (\x acc -> x * 2 : acc) [] [1 :: Int ..]))
    ]

takeWhileTests :: TestGroup
takeWhileTests = takeWhileCases "takeWhile" takeWhile

takeWhile2Tests :: TestGroup
takeWhile2Tests = takeWhileCases "takeWhile2 (foldr)" takeWhile2

-- | Same cases for both takeWhile implementations.
takeWhileCases :: String -> ((Int -> Bool) -> [Int] -> [Int]) -> TestGroup
takeWhileCases name tw =
  group
    name
    [ assertEqual "empty" [] (tw even [])
    , assertEqual "single kept" [2] (tw even [2])
    , assertEqual "single dropped" [] (tw even [1])
    , assertEqual "stops at first failure" [2, 4] (tw even xs)
    , assertEqual "all match" [2, 4] (tw even [2, 4])
    , assertEqual "vs Prelude" (P.takeWhile (< 6) xs) (tw (< 6) xs)
    , assertEqual "infinite, predicate fails" [1, 2] (tw (< 3) [1 ..])
    , assertEqual "infinite, predicate holds" [1, 2, 3] (take 3 (tw (> 0) [1 ..]))
    ]

dropWhileTests :: TestGroup
dropWhileTests =
  group
    "dropWhile"
    [ assertEqual "empty" [] (dropWhile even ([] :: [Int]))
    , assertEqual "single dropped" [] (dropWhile even [2 :: Int])
    , assertEqual "single kept" [1] (dropWhile even [1 :: Int])
    , assertEqual "drops prefix only" [5, 6, 1, 8] (dropWhile even xs)
    , assertEqual "vs Prelude" (P.dropWhile (< 6) xs) (dropWhile (< 6) xs)
    , assertEqual "infinite" [3, 4, 5] (take 3 (dropWhile (< 3) [1 :: Int ..]))
    ]

spanTests :: TestGroup
spanTests =
  group
    "span"
    [ assertEqual "empty" ([], []) (span even ([] :: [Int]))
    , assertEqual "single kept" ([2], []) (span even [2 :: Int])
    , assertEqual "single dropped" ([], [1]) (span even [1 :: Int])
    , assertEqual "splits at first failure" ([2, 4], [5, 6, 1, 8]) (span even xs)
    , assertEqual "vs Prelude" (P.span (< 6) xs) (span (< 6) xs)
    , assertEqual "infinite, fst lazy" [1, 2, 3] (take 3 (fst (span (> 0) [1 :: Int ..])))
    , assertEqual "infinite, snd lazy" [3, 4] (take 2 (snd (span (< 3) [1 :: Int ..])))
    ]

wordsTests :: TestGroup
wordsTests =
  group
    "words"
    [ assertEqual "empty" [] (words "")
    , assertEqual "only spaces" [] (words "   ")
    , assertEqual "single word" ["abc"] (words "abc")
    , assertEqual "leading and trailing spaces" ["ab", "cd"] (words "  ab cd  ")
    , assertEqual "multiple spaces" ["ab", "cd"] (words "ab    cd")
    , assertEqual "tabs and newlines" ["ab", "cd", "ef"] (words "ab\tcd\nef")
    , assertEqual "vs Prelude" (P.words text) (words text)
    , assertEqual "infinite" ["a", "a", "a"] (take 3 (words (cycle "a ")))
    ]
  where
    text = " the quick\tbrown  fox\n jumps  "
