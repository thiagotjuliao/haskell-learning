-- | Tests for "M01.ListPrelude": the shared cases from "M01.ListPreludeCases",
-- plus 'foldr', which only this version implements.
module M01.ListPreludeSpec (tests) where

import M01.ListPrelude
import M01.ListPreludeCases
import Testing.Runner
import Prelude hiding (dropWhile, filter, foldl, foldr, map, span, takeWhile, words, zipWith)
import qualified Prelude as P

tests :: [TestGroup]
tests =
  [ filterCases "filter" filter
  , mapCases "map" map
  , zipWithCases "zipWith" zipWith
  , foldlCases "foldl" foldl
  , foldrTests
  , takeWhileCases "takeWhile" takeWhile
  , dropWhileCases "dropWhile" dropWhile
  , spanCases "span" span
  , wordsCases "words" words
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
  where
    xs :: [Int]
    xs = [2, 4, 5, 6, 1, 8]
