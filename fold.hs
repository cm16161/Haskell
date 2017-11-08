module Fold where
import Prelude hiding (sum, product, length, and, or, all, any, filter, map)

sum :: [Integer] -> Integer
sum xs = foldr (f) k xs where
    f = (+)
    k = 0

product :: [Integer] -> Integer
product xs = foldr (*) 1 xs

and :: [Bool] -> Bool
and xs = foldr (&&) True xs

or :: [Bool] -> Bool
or xs = foldr (||) False xs

all :: (a -> Bool) -> [a] -> Bool
all f xs = and(map f xs)

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr ((&&).f) True xs

any :: (a -> Bool) -> [a] -> Bool
any f xs = or(map f xs)

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr ((||).f) True xs

length :: [a] -> Int
length xs = foldr f 0 xs where
  f :: a -> Int -> Int
  f x n = 1 + n

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (f p) [] xs where
  f :: (a -> Bool) -> a -> [a] -> [a]
  f p x xs
    | p x = x:xs
    | otherwise = xs

group :: Eq a => [a] -> [[a]]
group xs = foldr p [] xs where
  p y [] = [[y]]
  p y ((x:xs) : xss)
    | y == x = (y:x:xs):xss
    | otherwise = [y] : (x:xs) : xss

transpose :: [[a]] -> [[a]]
transpose xs  = foldr f k xs where
    f = zipWith (:)
    k = repeat []

map f = foldr ((:) . f) []

{-}
permute :: [a] -> [[a]]
sprinkle :: a -> [a] -> [[a]]
sprinkle (n) (x:xs) = foldr f k x:xs where
  k = []
  f (n) (x:xs)
    | n==x = (x:n:xs):k
    | otherwise = (n:x:xs):k

permute xs = foldr f k xs where
  f = (concat . map . sprinkle) (xs)
  k = []
-}
