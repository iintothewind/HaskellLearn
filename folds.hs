module Haskell.Folds () where

suml :: (Num a) => [a] -> a
suml = foldl (\acc x -> acc + x) 0

eleml :: (Eq a) => a -> [a] -> Bool
eleml x xs = foldl (\acc y -> if y == x then True else acc) False xs

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = reverse (foldl (\acc x -> f x : acc) [] xs)

mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

maxl :: (Ord a) => [a] -> a
maxl = foldl1 (\x acc -> if x > acc then x else acc)

revl :: [a] -> [a]
revl = foldl (\acc x -> x:acc) []

prd :: (Num a) => [a] -> a
prd = foldr1 (*)

had :: [a] -> a
had = foldr1 (\x _ -> x)

lst :: [a] -> a
lst = foldl1 (\_ x -> x)
