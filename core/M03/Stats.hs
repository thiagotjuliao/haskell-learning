-- | Module 3, exercise 5: count, sum, minimum, maximum and mean of a list in one pass,
-- as one composite monoid.
module M03.Stats (
  Stats,
  fromValue,
  getMean,
  stats,
  getCount,
  getMax,
  getMin,
  getSum,
) where

import M03.Monoids (Max (..), Min (..), Sum (..))

-- Stats count sum min max
data Stats a = Stats Int (Sum a) (Maybe (Min a)) (Maybe (Max a))
  deriving (Eq, Show)

instance (Num a, Ord a) => Semigroup (Stats a) where
  (<>) (Stats c1 s1 min1 max1) (Stats c2 s2 min2 max2) =
    Stats (c1 + c2) (s1 <> s2) (min1 <> min2) (max1 <> max2)

instance (Num a, Ord a) => Monoid (Stats a) where
  mempty = Stats 0 mempty mempty mempty

fromValue :: a -> Stats a
fromValue a = Stats 1 (Sum a) (Just (Min a)) (Just (Max a))

getCount :: Stats a -> Int
getCount (Stats c _ _ _) = c

getSum :: Stats a -> Sum a
getSum (Stats _ s _ _) = s

getMin :: Stats a -> Maybe (Min a)
getMin (Stats _ _ min' _) = min'

getMax :: Stats a -> Maybe (Max a)
getMax (Stats _ _ _ max') = max'

getMean :: (Real a) => Stats a -> Maybe Double
getMean (Stats 0 _ _ _) = Nothing
getMean (Stats c (Sum s) _ _) = Just (realToFrac s / fromIntegral c)

stats :: (Num a, Ord a) => [a] -> Stats a
stats = foldMap fromValue
