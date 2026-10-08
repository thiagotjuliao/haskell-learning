-- | Tests for "M02.Instances": every hand-written instance must agree with
-- the derived one on the same values. Manual values are converted to their
-- derived twin by pattern matching, not through the instances under test, so
-- a broken 'fromEnum' cannot hide a broken 'toEnum'.
--
-- Infinite ranges (@[d ..]@ is not, but @[d, d ..]@ is) are cut with 'take'.
-- Calls that throw ('toEnum' out of range, 'succ' of the last day) are left
-- out: the runner has no way to catch an exception.
module M02.InstancesSpec (tests) where

import M02.Instances
import Testing.Runner

tests :: [TestGroup]
tests = [dayClasses, dayRanges, userTests, shapeTests, treeTests]

-- | The derived twin of a manual day.
derivedDay :: DayOfWeek -> DayOfWeekDerived
derivedDay d = case d of
  Sun -> Sunday
  Mon -> Monday
  Tue -> Tuesday
  Wed -> Wednesday
  Thu -> Thursday
  Fri -> Friday
  Sat -> Saturday

-- | Listed by hand rather than with @[minBound .. maxBound]@, which would go
-- through the instances under test.
days :: [DayOfWeek]
days = [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

dayPairs :: [(DayOfWeek, DayOfWeek)]
dayPairs = [(a, b) | a <- days, b <- days]

dayClasses :: TestGroup
dayClasses =
  group
    "DayOfWeek: Eq, Ord, Show, Bounded"
    [ assertEqual "show" (map (show . derivedDay) days) (map show days)
    , assertEqual
        "show inside a constructor"
        (map (show . Just . derivedDay) days)
        (map (show . Just) days)
    , assertEqual
        "=="
        [derivedDay a == derivedDay b | (a, b) <- dayPairs]
        [a == b | (a, b) <- dayPairs]
    , assertEqual
        "compare"
        [compare (derivedDay a) (derivedDay b) | (a, b) <- dayPairs]
        [compare a b | (a, b) <- dayPairs]
    , assertEqual "minBound" minBound (derivedDay minBound)
    , assertEqual "maxBound" maxBound (derivedDay maxBound)
    ]

dayRanges :: TestGroup
dayRanges =
  group
    "DayOfWeek: Enum"
    [ assertEqual
        "fromEnum"
        (map (fromEnum . derivedDay) days)
        (map fromEnum days)
    , assertEqual
        "toEnum"
        (map toEnum [0 .. 6] :: [DayOfWeekDerived])
        (map (derivedDay . toEnum) [0 .. 6])
    , assertEqual
        "succ, all but the last"
        (map (succ . derivedDay) (init' days))
        (map (derivedDay . succ) (init' days))
    , assertEqual
        "pred, all but the first"
        (map (pred . derivedDay) (drop 1 days))
        (map (derivedDay . pred) (drop 1 days))
    , assertEqual
        "[a ..]"
        [[derivedDay a ..] | a <- days]
        [map derivedDay [a ..] | a <- days]
    , assertEqual
        "[a, b ..], every pair, both directions and a zero step"
        [take 10 [derivedDay a, derivedDay b ..] | (a, b) <- dayPairs]
        [take 10 (map derivedDay [a, b ..]) | (a, b) <- dayPairs]
    , assertEqual
        "[a .. b]"
        [[derivedDay a .. derivedDay b] | (a, b) <- dayPairs]
        [map derivedDay [a .. b] | (a, b) <- dayPairs]
    , assertEqual
        "[a, b .. c]"
        [ take 10 [derivedDay a, derivedDay b .. derivedDay c]
        | (a, b) <- dayPairs
        , c <- days
        ]
        [take 10 (map derivedDay [a, b .. c]) | (a, b) <- dayPairs, c <- days]
    ]
  where
    -- Total stand-in for 'init': 'days' is never empty, but the convention
    -- forbids the partial one anyway.
    init' :: [a] -> [a]
    init' xs = take (length xs - 1) xs

-- | Fields chosen to hit the edges of derived 'Show' and 'Ord': an empty
-- string, a string with quotes and a newline, and a negative number.
userFields :: [(String, String, Int)]
userFields =
  [ (n, e, a)
  | n <- ["", "ana", "bia", "say \"hi\"\n"]
  , e <- ["a@x.com", "b@x.com"]
  , a <- [-1, 0, 1]
  ]

-- | Each manual user next to its derived twin.
users :: [(User, UserDerived)]
users = [(User n e a, UserDerived n e a) | (n, e, a) <- userFields]

-- | The checks every manual type shares with its derived twin: 'show' alone
-- and as a constructor argument, then '==' and 'compare' on every pair. The
-- derived output is renamed before comparing, since the constructor names
-- differ.
agreesWithDerived ::
  (Eq m, Ord m, Show m, Eq d, Ord d, Show d) =>
  [(String, String)] ->
  [(m, d)] ->
  [TestResult]
agreesWithDerived names xs =
  [ assertEqual "show" [renamed names (show d) | (_, d) <- xs] [show m | (m, _) <- xs]
  , assertEqual
      "show inside a constructor"
      [renamed names (show (Just d)) | (_, d) <- xs]
      [show (Just m) | (m, _) <- xs]
  , assertEqual "==" [d1 == d2 | (d1, d2) <- dPairs] [m1 == m2 | (m1, m2) <- mPairs]
  , assertEqual "compare" [compare d1 d2 | (d1, d2) <- dPairs] [compare m1 m2 | (m1, m2) <- mPairs]
  ]
  where
    mPairs = [(m1, m2) | (m1, _) <- xs, (m2, _) <- xs]
    dPairs = [(d1, d2) | (_, d1) <- xs, (_, d2) <- xs]

-- | Replaces each derived constructor name with its manual one. Fields hold
-- only numbers and the sample strings, none of which contains a derived name.
renamed :: [(String, String)] -> String -> String
renamed names s = case s of
  [] -> []
  c : rest -> case [(to, after) | (from, to) <- names, Just after <- [splitPrefix from s]] of
    (to, after) : _ -> to ++ renamed names after
    [] -> c : renamed names rest
  where
    splitPrefix :: String -> String -> Maybe String
    splitPrefix [] xs = Just xs
    splitPrefix (p : ps) (x : xs) | p == x = splitPrefix ps xs
    splitPrefix _ _ = Nothing

userTests :: TestGroup
userTests =
  group
    "User: Eq, Ord, Show"
    ( [ assertEqual
          "show, by hand"
          "User \"Ana\" \"ana@x.com\" (-3)"
          (show (User "Ana" "ana@x.com" (-3)))
      , assertEqual
          "show inside a constructor, by hand"
          "Just (User \"a\" \"b\" 1)"
          (show (Just (User "a" "b" 1)))
      ]
        ++ agreesWithDerived [("UserDerived", "User")] users
    )

-- | Every shape with coordinates in @[-1, 0, 1]@, so that 'Ord' meets ties on
-- the first field and 'Show' meets negative fields.
shapes :: [(Shape, ShapeD)]
shapes =
  (Point, PointD)
    : [(Circle x y, CircleD x y) | x <- coords, y <- coords]
    ++ [(Square x y, SquareD x y) | x <- coords, y <- coords]
  where
    coords = [-1, 0, 1]

shapeTests :: TestGroup
shapeTests =
  group
    "Shape: Eq, Ord, Show"
    ( [ assertEqual "show, no parentheses on Point" "Just Point" (show (Just Point))
      , assertEqual "show, by hand" "[Point,Circle (-1) 0]" (show [Point, Circle (-1) 0])
      ]
        ++ agreesWithDerived
          [("PointD", "Point"), ("CircleD", "Circle"), ("SquareD", "Square")]
          shapes
    )

-- | The derived twin of a manual tree.
derivedTree :: Tree a -> TreeD a
derivedTree t = case t of
  Leaf a -> LeafD a
  Branch l r -> BranchD (derivedTree l) (derivedTree r)

-- | Trees up to depth 2 over @[-1, 0]@: leaves against branches, ties on the
-- left subtree, and negative leaves under two levels of constructors.
trees :: [(Tree Int, TreeD Int)]
trees = [(t, derivedTree t) | t <- leaves ++ branches ++ deep]
  where
    leaves = [Leaf (-1), Leaf 0]
    branches = [Branch l r | l <- leaves, r <- leaves]
    deep = [Branch b l | b <- branches, l <- leaves] ++ [Branch l b | l <- leaves, b <- branches]

treeTests :: TestGroup
treeTests =
  group
    "Tree: Eq, Ord, Show"
    ( assertEqual
        "show, by hand"
        "Branch (Leaf 1) (Branch (Leaf (-2)) (Leaf 3))"
        (show (Branch (Leaf 1) (Branch (Leaf (-2)) (Leaf (3 :: Int)))))
        : agreesWithDerived [("LeafD", "Leaf"), ("BranchD", "Branch")] trees
    )
