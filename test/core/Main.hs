module Main (main) where

import qualified M01.ListPreludeSpec as ListPrelude
import Testing.Runner

tests :: [TestGroup]
tests =
  sanity
    ++ ListPrelude.tests

-- | Smoke tests for the runner itself.
sanity :: [TestGroup]
sanity =
  [ group "arith" [assertEqual "sum" 4 (2 + 2), assertEqual "mul" 1 (1 * 1)]
  , group "strings" [assertEqual "concat" "abc" ("ab" ++ "c")]
  ]

main :: IO ()
main = do
  putStrLn "[core] Running tests...\n"
  runTests tests
