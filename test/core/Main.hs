module Main (main) where

import qualified M01.ExprSpec as Expr
import qualified M01.ListPreludeFoldrSpec as ListPreludeFoldr
import qualified M01.ListPreludePointFreeSpec as ListPreludePointFree
import qualified M01.ListPreludeSpec as ListPrelude
import qualified M02.FractionSpec as Fraction
import qualified M02.InstancesSpec as Instances
import qualified M02.MatrixSpec as Matrix
import Testing.Runner

tests :: [TestGroup]
tests =
  sanity
    ++ ListPrelude.tests
    ++ ListPreludeFoldr.tests
    ++ ListPreludePointFree.tests
    ++ Expr.tests
    ++ Instances.tests
    ++ Fraction.tests
    ++ Matrix.tests

-- | Smoke tests for the runner itself.
sanity :: [TestGroup]
sanity =
  [ group "arith" [assertEqual "sum" 4 (2 + 2 :: Int), assertEqual "mul" 1 (1 * 1 :: Int)]
  , group "strings" [assertEqual "concat" "abc" ("ab" ++ "c")]
  ]

main :: IO ()
main = do
  putStrLn "[core] Running tests...\n"
  runTests tests
