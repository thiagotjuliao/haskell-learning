-- | Tests for "M01.ListPreludeFoldr".
--
-- The same cases as "M01.ListPreludeSpec": rewriting a function with 'foldr'
-- must not change what it returns, including on infinite input, where the
-- lazy patterns in the implementations are what keep the fold from running
-- to the end of the list.
module M01.ListPreludeFoldrSpec (tests) where

import M01.ListPreludeFoldr
import Testing.Runner
import Prelude hiding (dropWhile, filter, foldl, map, span, takeWhile, words, zipWith)
import qualified Prelude as P

tests :: [TestGroup]
tests =
  [ filterTests
  , mapTests
  , zipWithTests
  , foldlTests
  , takeWhileTests
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
    "filter (foldr)"
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
    "map (foldr)"
    [ assertEqual "empty" [] (map show ([] :: [Int]))
    , assertEqual "single" ["1"] (map show [1 :: Int])
    , assertEqual "vs Prelude" (P.map (* 2) xs) (map (* 2) xs)
    , assertEqual "infinite" [2, 4, 6] (take 3 (map (* 2) [1 :: Int ..]))
    ]

zipWithTests :: TestGroup
zipWithTests =
  group
    "zipWith (foldr)"
    [ assertEqual "both empty" [] (zipWith (+) [] ([] :: [Int]))
    , assertEqual "left empty" [] (zipWith (+) [] [1 :: Int])
    , assertEqual "right empty" [] (zipWith (+) [1 :: Int] [])
    , assertEqual "single" [3] (zipWith (+) [1] [2 :: Int])
    , assertEqual "left shorter" [4] (zipWith (+) [1] [3, 4 :: Int])
    , assertEqual "right shorter" [4] (zipWith (+) [1, 2] [3 :: Int])
    , assertEqual "vs Prelude" (P.zipWith (,) xs "abc") (zipWith (,) xs "abc")
    , -- The fold walks the left list, so each side runs out through a
      -- different equation of step.
      assertEqual "left infinite" [11, 22] (zipWith (+) [1 ..] [10, 20 :: Int])
    , assertEqual "right infinite" [11, 22] (zipWith (+) [10, 20] [1 :: Int ..])
    , assertEqual "both infinite" [2, 4, 6] (take 3 (zipWith (+) [1 ..] [1 :: Int ..]))
    ]

foldlTests :: TestGroup
foldlTests =
  group
    "foldl (foldr)"
    [ assertEqual "empty returns seed" 0 (foldl (+) 0 ([] :: [Int]))
    , assertEqual "single" 5 (foldl (+) 0 [5 :: Int])
    , assertEqual "sum" 55 (foldl (+) 0 [1 .. 10 :: Int])
    , assertEqual "reverses with flip (:)" "cba" (foldl (flip (:)) [] "abc")
    , assertEqual "left-associative" (((0 - 1) - 2) - 3) (foldl (-) 0 [1, 2, 3 :: Int])
    , assertEqual "vs Prelude" (P.foldl (-) 100 xs) (foldl (-) 100 xs)
    ]

takeWhileTests :: TestGroup
takeWhileTests =
  group
    "takeWhile (foldr)"
    [ assertEqual "empty" [] (takeWhile even ([] :: [Int]))
    , assertEqual "single kept" [2] (takeWhile even [2 :: Int])
    , assertEqual "single dropped" [] (takeWhile even [1 :: Int])
    , assertEqual "stops at first failure" [2, 4] (takeWhile even xs)
    , assertEqual "all match" [2, 4] (takeWhile even [2, 4 :: Int])
    , assertEqual "vs Prelude" (P.takeWhile (< 6) xs) (takeWhile (< 6) xs)
    , assertEqual "infinite, predicate fails" [1, 2] (takeWhile (< 3) [1 :: Int ..])
    , assertEqual "infinite, predicate holds" [1, 2, 3] (take 3 (takeWhile (> 0) [1 :: Int ..]))
    ]

dropWhileTests :: TestGroup
dropWhileTests =
  group
    "dropWhile (foldr)"
    [ assertEqual "empty" [] (dropWhile even ([] :: [Int]))
    , assertEqual "single dropped" [] (dropWhile even [2 :: Int])
    , assertEqual "single kept" [1] (dropWhile even [1 :: Int])
    , assertEqual "drops prefix only" [5, 6, 1, 8] (dropWhile even xs)
    , -- Elements after the first failure that satisfy p must stay.
      assertEqual "keeps later matches" [1, 2, 4] (dropWhile even [2, 1, 2, 4 :: Int])
    , assertEqual "vs Prelude" (P.dropWhile (< 6) xs) (dropWhile (< 6) xs)
    , assertEqual "infinite" [3, 4, 5] (take 3 (dropWhile (< 3) [1 :: Int ..]))
    ]

spanTests :: TestGroup
spanTests =
  group
    "span (foldr)"
    [ assertEqual "empty" ([], []) (span even ([] :: [Int]))
    , assertEqual "single kept" ([2], []) (span even [2 :: Int])
    , assertEqual "single dropped" ([], [1]) (span even [1 :: Int])
    , assertEqual "splits at first failure" ([2, 4], [5, 6, 1, 8]) (span even xs)
    , -- snd is rebuilt as valid ++ invalid, so later runs must come back whole.
      assertEqual "keeps later runs" ([2], [1, 2, 4, 3]) (span even [2, 1, 2, 4, 3 :: Int])
    , assertEqual "vs Prelude" (P.span (< 6) xs) (span (< 6) xs)
    , assertEqual "infinite, fst lazy" [1, 2, 3] (take 3 (fst (span (> 0) [1 :: Int ..])))
    , assertEqual "infinite, snd lazy" [3, 4] (take 2 (snd (span (< 3) [1 :: Int ..])))
    ]

wordsTests :: TestGroup
wordsTests =
  group
    "words (foldr)"
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
