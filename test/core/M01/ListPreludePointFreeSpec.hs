-- | Tests for "M01.ListPreludePointFree": the shared cases from
-- "M01.ListPreludeCases", for the functions that have a point-free version.
module M01.ListPreludePointFreeSpec (tests) where

import M01.ListPreludeCases
import M01.ListPreludePointFree
import Testing.Runner (TestGroup)
import Prelude hiding (filter, foldl, map, takeWhile)

tests :: [TestGroup]
tests =
  [ filterCases "filter (point-free)" filter
  , mapCases "map (point-free)" map
  , foldlCases "foldl (point-free)" foldl
  , takeWhileCases "takeWhile (point-free)" takeWhile
  ]
