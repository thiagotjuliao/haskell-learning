-- | Module 1, exercise 3: the list Prelude in point-free style, wherever it
-- stays readable.
--
-- Only the 'foldr' steps lose their points here. zipWith, dropWhile, span and
-- words are left out: their steps take the list or the pair apart and use the
-- pieces in different places, which point-free can only reach through
-- 'uncurry' and 'flip' gymnastics. The findings are in docs/notes/M01.md.
module M01.ListPreludePointFree (map, filter, takeWhile, foldl) where

import M01.ListPrelude (foldr)
import Prelude hiding (dropWhile, filter, foldl, foldr, map, span, takeWhile, words, zipWith)

-- >>> map (* 2) [1, 2, 3 :: Int]
-- [2,4,6]
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

-- The step takes only the element and returns what to do with the filtered
-- rest: prepend the element, or leave the rest as it is.
--
-- >>> filter even [2, 4, 5, 6, 1 :: Int]
-- [2,4,6]
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
  where
    f x
      | p x = (:) x
      | otherwise = id

-- As in filter, except that a failing element discards the rest. 'mempty'
-- works here too, through the Monoid instance of functions, but 'const []'
-- says what happens without making the reader work out the type.
--
-- >>> takeWhile even [2, 4, 5, 6 :: Int]
-- [2,4]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr f []
  where
    f x
      | p x = (:) x
      | otherwise = const []

-- Only the step is point-free. foldl f = flip (foldr step id) also compiles,
-- but it hides that the chain of functions built by foldr is applied to z.
--
-- >>> foldl (\b a -> a : b) [] ["a", "b", "c"]
-- ["c","b","a"]
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = foldr step id xs z
  where
    step x k = k . flip f x
