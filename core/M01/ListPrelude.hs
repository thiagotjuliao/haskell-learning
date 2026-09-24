-- | Module 1, exercise 2: the list Prelude reimplemented with explicit recursion.
module M01.ListPrelude where

import Prelude hiding (filter, foldl, foldr, map, span, takeWhile, words, zipWith)

-- >>> filter (\a -> a > 2) [1, 2, 3, 4]
-- [3,4]
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (h : t)
  | p h = h : filter p t
  | otherwise = filter p t

-- >>> map (\a -> show a) [1..10]
-- ["1","2","3","4","5","6","7","8","9","10"]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (h : t) = f h : map f t

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

-- >>> foldl (\b a -> b + a) 0 [1..10]
-- 55

-- >>> foldl (\b a -> a : b) [] ["a", "b", "c"]
-- ["c","b","a"]
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

-- >>> foldr (\a b -> a : b) [] ["a", "b", "c"]
-- ["a","b","c"]
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

-- >>> takeWhile (even) [2, 4, 5, 6]
-- [2,4]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = foldr f [] xs
  where
    f a b
      | p a = a : b
      | otherwise = []
