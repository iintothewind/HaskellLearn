module Haskell.HighOrderFunctions () where

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
multWithEighteen = multTwoWithNine 2

cmpWithHundred :: (Num a, Ord a) => a -> Ordering
cmpWithHundred = compare 100

divByTen :: (Floating a) => a -> a
divByTen = (/10)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zpWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zpWith _ [] _          = []
zpWith _ _ []          = []
zpWith f (x:xs) (y:ys) = f x y : zpWith f xs ys

flp :: (a -> b -> c) -> (b -> a -> c)
flp f x y = f y x

