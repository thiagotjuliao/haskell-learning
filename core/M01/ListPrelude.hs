-- | Module 1, exercise 2: the list Prelude reimplemented with explicit recursion.
module M01.ListPrelude where

import Data.Char (isSpace)
import Prelude hiding (dropWhile, filter, foldl, foldr, map, span, takeWhile, words, zipWith)

-- >>> filter (\a -> a > 2) [1, 2, 3, 4 :: Int]
-- [3,4]
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

-- >>> map (\a -> show a) [1..10 :: Int]
-- ["1","2","3","4","5","6","7","8","9","10"]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- >>> zipWith (+) [1, 2 :: Int] [3, 4 :: Int]
-- >>> zipWith (+) [1 :: Int] [2, 3 :: Int]
-- >>> zipWith (+) [1, 2 :: Int] [3 :: Int]
-- >>> zipWith (+) [1, 2 :: Int] []
-- [4,6]
-- [3]
-- [4]
-- []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- >>> foldl (\b a -> b + a) 0 [1..10 :: Int]
-- >>> foldl (\b a -> a : b) [] ["a", "b", "c"]
-- 55
-- ["c","b","a"]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

-- >>> foldr (\a b -> a : b) [] ["a", "b", "c"]
-- ["a","b","c"]
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

-- >>> takeWhile (even) [2, 4, 5, 6 :: Int]
-- [2,4]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

-- >>> takeWhile2 (even) [2, 4, 5, 6 :: Int]
-- [2,4]
takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p = foldr f []
  where
    f x xs
      | p x = x : xs
      | otherwise = []

-- >>> dropWhile (even) [2, 4, 5, 6, 1 :: Int]
-- [5,6,1]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = dropWhile p xs
  | otherwise = x : xs

-- >>> span (even) [2, 4, 5, 6, 1 :: Int]
-- ([2,4],[5,6,1])
span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span p l@(x : xs)
  | p x =
      let
        (y, ys) = span p xs
       in
        (x : y, ys)
  | otherwise = ([], l)

-- >>> words " ab cd ef   gh  "
-- ["ab","cd","ef","gh"]
words :: String -> [String]
words [] = []
words s
  | w == "" = []
  | otherwise = w : words r
  where
    (w, r) = span (not . isSpace) $ snd $ span (isSpace) s
