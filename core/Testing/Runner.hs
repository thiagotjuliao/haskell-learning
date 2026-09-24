-- | Hand-rolled test runner (module 1): assertEqual plus pass/fail counting.
module Testing.Runner (
  TestResult,
  TestGroup,
  assertEqual,
  group,
  runTests,
) where

import Control.Monad (when)
import System.Exit (exitFailure)

data TestResult = Success String | Failure String String
  deriving (Show)

data TestGroup = TestGroup {key :: String, tests :: [TestResult]}
  deriving (Show)

data TestMetrics = TestMetrics {passed :: Int, failed :: Int}
  deriving (Show)

instance Semigroup TestMetrics where
  (TestMetrics p1 f1) <> (TestMetrics p2 f2) = TestMetrics (p1 + p2) (f1 + f2)

instance Monoid TestMetrics where
  mempty = TestMetrics 0 0

data TestGroupResult = TestGroupResult String [String] TestMetrics
  deriving (Show)

toMetric :: TestResult -> TestMetrics
toMetric (Success _) = TestMetrics 1 0
toMetric (Failure _ _) = TestMetrics 0 1

render :: TestGroupResult -> String
render (TestGroupResult n e (TestMetrics p f)) =
  unlines
    ( ["---< " ++ n ++ " >---"]
        ++ e
        ++ [ "Passed: " ++ show p
           , "Failed: " ++ show f
           ]
    )

assertEqual :: (Eq a, Show a) => String -> a -> a -> TestResult
assertEqual name expected obtained
  | expected == obtained = Success name
  | otherwise = Failure name msg
  where
    msg =
      "expected "
        ++ show expected
        ++ " but got "
        ++ show obtained
        ++ " instead"

group :: String -> [TestResult] -> TestGroup
group = TestGroup

failedMsgs :: [TestResult] -> [String]
failedMsgs ts = ["[" ++ n ++ "] " ++ e | Failure n e <- ts]

metrics :: [TestResult] -> TestMetrics
metrics ts = foldMap toMetric ts

testGroupResult :: TestGroup -> TestGroupResult
testGroupResult (TestGroup k ts) = TestGroupResult k (failedMsgs ts) (metrics ts)

runTests :: [TestGroup] -> IO ()
runTests ts = do
  let
    trs = map testGroupResult ts
    msgs = map render trs
    (TestMetrics p f) = mconcat [m | TestGroupResult _ _ m <- trs]
    result = "Final Result: " ++ show p ++ " passed / " ++ show f ++ " failed\n"
  mapM_ putStrLn msgs
  putStrLn "-------------------------------------------"
  putStrLn result
  when (f > 0) exitFailure
