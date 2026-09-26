-- | Module 1, exercise 2: the list Prelude again, every function written with 'foldr'.
module M01.ListPreludeFoldr (filter, map, zipWith, foldl, takeWhile, dropWhile, span, words) where

import Data.Char (isSpace)
import M01.ListPrelude (foldr)
import Prelude hiding (dropWhile, filter, foldl, foldr, map, span, takeWhile, words, zipWith)

-- >>> filter (\a -> a > 2) [1, 5, 2, 3, 4 :: Int]
-- [5,3,4]
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
  where
    f x xs
      | p x = x : xs
      | otherwise = xs

-- >>> map (\a -> show a) [1..10 :: Int]
-- ["1","2","3","4","5","6","7","8","9","10"]
map :: (a -> b) -> [a] -> [b]
map f = foldr (\a b -> f a : b) []

-- foldr walks a single list, so it walks the first one and builds a function
-- that still waits for the second: each step takes what is left of ys, pairs
-- its head with x and hands the tail to the step for the rest of xs (k). The
-- seed, const [], is reached when xs runs out, whatever is left of ys.
--
-- >>> zipWith (+) [1, 2] [3]
-- [4]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr step (const [])
  where
    -- step :: a -> ([b] -> [c]) -> [b] -> [c]
    step x k (y : ys) = f x y : k ys
    step _ _ [] = []

-- foldr combines from the right, but foldl needs the accumulator coming from
-- the left, so foldr builds a chain of functions instead of a value: each step
-- receives the accumulator so far, applies f to it and passes the result on to
-- the step for the rest of the list (k); id ends the chain. Applying the chain
-- to z runs it left to right:
--
--   foldl f z [x1, x2] = step x1 (step x2 id) z = id (f (f z x1) x2)
--
-- >>> foldl (\b a -> a : b) [] ["a", "b", "c"]
-- ["c","b","a"]
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = foldr step id xs z
  where
    -- step :: a -> (acc -> acc) -> acc -> acc
    step x k acc = k (f acc x)

-- >>> takeWhile (even) [2, 4, 5, 6 :: Int]
-- [2,4]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr f []
  where
    f x xs
      | p x = x : xs
      | otherwise = []

-- At each element foldr only has the result for the rest of the list, but once
-- p fails the answer is x followed by the rest *unchanged*, which the recursive
-- result may already have dropped from. So the fold carries both: dropWhile p
-- of the suffix, and the suffix itself, rebuilt with (:).
--
-- The lazy pattern (~) keeps it working on infinite lists: matching the pair
-- strictly would force the fold all the way to the end of the list.
--
-- >>> dropWhile (even) [2, 4, 5, 6, 1 :: Int]
-- [5,6,1]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = fst . foldr f ([], [])
  where
    f x ~(dropped, rest) = (if p x then dropped else x : rest, x : rest)

-- The fold carries span p of the suffix. While p holds, x joins the prefix.
-- When p fails the prefix ends at x, and the second half is x followed by the
-- whole original suffix. Unlike dropWhile there is no need to carry that
-- suffix: span only splits a list in two, so it is valid ++ invalid. (++)
-- copies each element at most once, so span stays O(n). The ~ is there for
-- infinite lists, as in dropWhile.
--
-- >>> span (even) [2, 4, 5, 6, 1 :: Int]
-- ([2,4],[5,6,1])
span :: (a -> Bool) -> [a] -> ([a], [a])
span p = foldr f ([], [])
  where
    f x ~(valid, invalid)
      | p x = (x : valid, invalid)
      | otherwise = ([], x : valid ++ invalid)

-- The fold carries the word being built and the finished words to its right.
-- Going right to left, each character is consed onto the front of the current
-- word. A space closes the current word and starts an empty one; flush drops
-- empty words, so runs of spaces and leading or trailing spaces yield no "".
-- The first word of the text is still open when the fold ends, hence the final
-- flush. The ~ is there for infinite strings, as in dropWhile.
--
-- >>> words " ab cd ef   gh  "
-- ["ab","cd","ef","gh"]
words :: String -> [String]
words = flush . foldr step ("", [])
  where
    -- step :: Char -> (String, [String]) -> (String, [String])
    step c ~(w, ws)
      | isSpace c = ("", flush (w, ws))
      | otherwise = (c : w, ws)
    -- flush :: (String, [String]) -> [String]
    flush (w, ws) = if null w then ws else w : ws
