{-# LANGUAGE InstanceSigs #-}

-- | Module 2, exercise 1: instances written by hand next to derived copies of the same types.
module M02.Instances (
  DayOfWeekDerived (..),
  DayOfWeek (..),
  UserDerived (..),
  User (..),
  ShapeD (..),
  Shape (..),
  TreeD (..),
  Tree (..),
) where

import Common.Utils (thenCompare)

-- | A: enumeration
data DayOfWeekDerived
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Show, Enum, Bounded)

data DayOfWeek
  = Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat

toInt :: DayOfWeek -> Int
toInt d =
  case d of
    Sun -> 0
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6

instance Show DayOfWeek where
  show :: DayOfWeek -> String
  show d =
    case d of
      Sun -> "Sunday"
      Mon -> "Monday"
      Tue -> "Tuesday"
      Wed -> "Wednesday"
      Thu -> "Thursday"
      Fri -> "Friday"
      Sat -> "Saturday"

instance Eq DayOfWeek where
  (==) :: DayOfWeek -> DayOfWeek -> Bool
  (==) self other = toInt self == toInt other

instance Ord DayOfWeek where
  (<=) :: DayOfWeek -> DayOfWeek -> Bool
  (<=) self other = toInt self <= toInt other

instance Enum DayOfWeek where
  toEnum :: Int -> DayOfWeek
  toEnum n =
    case n of
      0 -> Sun
      1 -> Mon
      2 -> Tue
      3 -> Wed
      4 -> Thu
      5 -> Fri
      6 -> Sat
      _ -> error "day out of range (0,6)"

  fromEnum :: DayOfWeek -> Int
  fromEnum = toInt

  enumFrom :: DayOfWeek -> [DayOfWeek]
  enumFrom d = [d .. Sat]

  enumFromThen :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
  enumFromThen d1 d2 = if d1 > d2 then [d1, d2 .. minBound] else [d1, d2 .. maxBound]

instance Bounded DayOfWeek where
  minBound :: DayOfWeek
  minBound = Sun

  maxBound :: DayOfWeek
  maxBound = Sat

-- | B: product type
data UserDerived = UserDerived String String Int
  deriving (Show, Eq, Ord)

data User = User String String Int

instance Eq User where
  (==) (User n1 e1 a1) (User n2 e2 a2) = n1 == n2 && e1 == e2 && a1 == a2

instance Show User where
  showsPrec d (User n e a) =
    showParen
      (d > 10)
      ( showString "User "
          . showsPrec 11 n
          . showString " "
          . showsPrec 11 e
          . showString " "
          . showsPrec 11 a
      )

instance Ord User where
  compare (User n1 e1 a1) (User n2 e2 a2) =
    compare n1 n2 `thenCompare` compare e1 e2 `thenCompare` compare a1 a2

-- | C: type with several constructors
data ShapeD
  = PointD
  | CircleD Int Int
  | SquareD Int Int
  deriving (Eq, Show, Ord)

data Shape
  = Point
  | Circle Int Int
  | Square Int Int

instance Eq Shape where
  (==) self other =
    case (self, other) of
      (Point, Point) -> True
      (Circle x1 y1, Circle x2 y2) -> x1 == x2 && y1 == y2
      (Square x1 y1, Square x2 y2) -> x1 == x2 && y1 == y2
      _ -> False

instance Ord Shape where
  compare s1 s2 =
    case (s1, s2) of
      (Point, Point) -> EQ
      (Point, _) -> LT
      (_, Point) -> GT
      (Circle x1 y1, Circle x2 y2) ->
        compare x1 x2 `thenCompare` compare y1 y2
      (Circle _ _, _) -> LT
      (Square x1 y1, Square x2 y2) ->
        compare x1 x2 `thenCompare` compare y1 y2
      (Square _ _, _) -> GT

instance Show Shape where
  showsPrec d s =
    case s of
      Point -> showString "Point"
      Circle x y -> showParen (d > 10) $ showString "Circle " . showsPrec 11 x . showString " " . showsPrec 11 y
      Square x y -> showParen (d > 10) $ showString "Square " . showsPrec 11 x . showString " " . showsPrec 11 y

-- | D: Recursive type
data TreeD a
  = LeafD a
  | BranchD (TreeD a) (TreeD a)
  deriving (Eq, Show, Ord)

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

instance (Eq a) => Eq (Tree a) where
  (==) self other =
    case (self, other) of
      (Leaf a, Leaf b) -> a == b
      (Branch lhs1 rhs1, Branch lhs2 rhs2) -> lhs1 == lhs2 && rhs1 == rhs2
      _ -> False

instance (Ord a) => Ord (Tree a) where
  compare self other =
    case (self, other) of
      (Leaf a, Leaf b) -> compare a b
      (Leaf _, _) -> LT
      (Branch lhs1 rhs1, Branch lhs2 rhs2) ->
        compare lhs1 lhs2 `thenCompare` compare rhs1 rhs2
      (Branch _ _, _) -> GT

instance (Show a) => Show (Tree a) where
  showsPrec d t =
    showParen (d > 10) $
      case t of
        Leaf a -> showString "Leaf " . showsPrec 11 a
        Branch lhs rhs -> showString "Branch " . showsPrec 11 lhs . showString " " . showsPrec 11 rhs
