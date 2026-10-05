-- | Tests for "M01.ListPreludeFoldr": the shared cases from
-- "M01.ListPreludeCases", since rewriting a function with 'foldr' must not
-- change what it returns.
module M01.ListPreludeFoldrSpec (tests) where

import M01.ListPreludeCases
import M01.ListPreludeFoldr
import Testing.Runner (TestGroup)
import Prelude hiding (dropWhile, filter, foldl, map, span, takeWhile, words, zipWith)

tests :: [TestGroup]
tests =
  [ filterCases "filter (foldr)" filter
  , mapCases "map (foldr)" map
  , zipWithCases "zipWith (foldr)" zipWith
  , foldlCases "foldl (foldr)" foldl
  , takeWhileCases "takeWhile (foldr)" takeWhile
  , dropWhileCases "dropWhile (foldr)" dropWhile
  , spanCases "span (foldr)" span
  , wordsCases "words (foldr)" words
  ]
