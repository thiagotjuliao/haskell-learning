-- | The test cases for the list Prelude, written once and run against every
-- implementation: each @*Cases@ function takes a group name and the function
-- under test.
--
-- Each function is checked on edge cases (empty list, single element),
-- against the real Prelude (imported qualified as @P@), and, where the
-- function should cope with it, on an infinite input. On infinite input the
-- 'foldr' versions only pass thanks to their lazy patterns, which keep the
-- fold from running to the end of the list.
--
-- The function under test is a parameter with a monomorphic type, so every
-- case of a function uses the same element types: testing @map show@ and
-- @map (* 2)@ through one parameter would need a rank-2 type (module 10).
module M01.ListPreludeCases (
  filterCases,
  mapCases,
  zipWithCases,
  foldlCases,
  takeWhileCases,
  dropWhileCases,
  spanCases,
  wordsCases,
) where

import Testing.Runner (TestGroup, assertEqual, group)
import Prelude
import qualified Prelude as P (dropWhile, filter, foldl, map, span, takeWhile, words, zipWith)

-- | Shared sample input: mixes evens and odds, with the predicate
-- flipping in the middle of the list.
xs :: [Int]
xs = [2, 4, 5, 6, 1, 8]

filterCases :: String -> ((Int -> Bool) -> [Int] -> [Int]) -> TestGroup
filterCases name fl =
  group
    name
    [ assertEqual "empty" [] (fl even [])
    , assertEqual "single kept" [2] (fl even [2])
    , assertEqual "single dropped" [] (fl even [1])
    , assertEqual "none match" [] (fl (> 100) xs)
    , assertEqual "vs Prelude" (P.filter even xs) (fl even xs)
    , assertEqual "infinite" [2, 4, 6] (take 3 (fl even [1 ..]))
    ]

mapCases :: String -> ((Int -> Int) -> [Int] -> [Int]) -> TestGroup
mapCases name mp =
  group
    name
    [ assertEqual "empty" [] (mp (+ 1) [])
    , assertEqual "single" [2] (mp (+ 1) [1])
    , assertEqual "vs Prelude" (P.map (* 2) xs) (mp (* 2) xs)
    , assertEqual "infinite" [2, 4, 6] (take 3 (mp (* 2) [1 ..]))
    ]

zipWithCases :: String -> ((Int -> Int -> Int) -> [Int] -> [Int] -> [Int]) -> TestGroup
zipWithCases name zw =
  group
    name
    [ assertEqual "both empty" [] (zw (+) [] [])
    , assertEqual "left empty" [] (zw (+) [] [1])
    , assertEqual "right empty" [] (zw (+) [1] [])
    , assertEqual "single" [3] (zw (+) [1] [2])
    , assertEqual "left shorter" [4] (zw (+) [1] [3, 4])
    , assertEqual "right shorter" [4] (zw (+) [1, 2] [3])
    , assertEqual "vs Prelude" (P.zipWith (-) xs [10, 20, 30]) (zw (-) xs [10, 20, 30])
    , -- The foldr version walks the left list, so each side runs out through
      -- a different equation of its step.
      assertEqual "left infinite" [11, 22] (zw (+) [1 ..] [10, 20])
    , assertEqual "right infinite" [11, 22] (zw (+) [10, 20] [1 ..])
    , assertEqual "both infinite" [2, 4, 6] (take 3 (zw (+) [1 ..] [1 ..]))
    ]

foldlCases :: String -> ((Int -> Int -> Int) -> Int -> [Int] -> Int) -> TestGroup
foldlCases name fl =
  group
    name
    [ assertEqual "empty returns seed" 0 (fl (+) 0 [])
    , assertEqual "single" 5 (fl (+) 0 [5])
    , assertEqual "sum" 55 (fl (+) 0 [1 .. 10])
    , -- Each step shifts what came before, so the digits come out in the
      -- order of the list only if it is consumed from the left.
      assertEqual "consumes from the left" 123 (fl (\acc d -> acc * 10 + d) 0 [1, 2, 3])
    , assertEqual "left-associative" (((0 - 1) - 2) - 3) (fl (-) 0 [1, 2, 3])
    , assertEqual "vs Prelude" (P.foldl (-) 100 xs) (fl (-) 100 xs)
    ]

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

dropWhileCases :: String -> ((Int -> Bool) -> [Int] -> [Int]) -> TestGroup
dropWhileCases name dw =
  group
    name
    [ assertEqual "empty" [] (dw even [])
    , assertEqual "single dropped" [] (dw even [2])
    , assertEqual "single kept" [1] (dw even [1])
    , assertEqual "drops prefix only" [5, 6, 1, 8] (dw even xs)
    , -- Elements after the first failure that satisfy p must stay.
      assertEqual "keeps later matches" [1, 2, 4] (dw even [2, 1, 2, 4])
    , assertEqual "vs Prelude" (P.dropWhile (< 6) xs) (dw (< 6) xs)
    , assertEqual "infinite" [3, 4, 5] (take 3 (dw (< 3) [1 ..]))
    ]

spanCases :: String -> ((Int -> Bool) -> [Int] -> ([Int], [Int])) -> TestGroup
spanCases name sp =
  group
    name
    [ assertEqual "empty" ([], []) (sp even [])
    , assertEqual "single kept" ([2], []) (sp even [2])
    , assertEqual "single dropped" ([], [1]) (sp even [1])
    , assertEqual "splits at first failure" ([2, 4], [5, 6, 1, 8]) (sp even xs)
    , -- The foldr version rebuilds snd as valid ++ invalid, so later runs
      -- must come back whole.
      assertEqual "keeps later runs" ([2], [1, 2, 4, 3]) (sp even [2, 1, 2, 4, 3])
    , assertEqual "vs Prelude" (P.span (< 6) xs) (sp (< 6) xs)
    , assertEqual "infinite, fst lazy" [1, 2, 3] (take 3 (fst (sp (> 0) [1 ..])))
    , assertEqual "infinite, snd lazy" [3, 4] (take 2 (snd (sp (< 3) [1 ..])))
    ]

wordsCases :: String -> (String -> [String]) -> TestGroup
wordsCases name ws =
  group
    name
    [ assertEqual "empty" [] (ws "")
    , assertEqual "only spaces" [] (ws "   ")
    , assertEqual "single word" ["abc"] (ws "abc")
    , assertEqual "leading and trailing spaces" ["ab", "cd"] (ws "  ab cd  ")
    , assertEqual "multiple spaces" ["ab", "cd"] (ws "ab    cd")
    , assertEqual "tabs and newlines" ["ab", "cd", "ef"] (ws "ab\tcd\nef")
    , assertEqual "vs Prelude" (P.words text) (ws text)
    , assertEqual "infinite" ["a", "a", "a"] (take 3 (ws (cycle "a ")))
    ]
  where
    text = " the quick\tbrown  fox\n jumps  "
