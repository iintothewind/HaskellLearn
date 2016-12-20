module Haskell.HighOrderFunctions () where

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
multWithEighteen = multTwoWithNine 2

cmpWithHundred :: (Num a, Ord a) => a -> Ordering
cmpWithHundred = compare 100

divByTen :: (Floating a) => a -> a
divByTen x= (/10)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])


