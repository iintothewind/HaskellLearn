module Haskell.HighOrderFunctions () where
import qualified Data.List as List
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

mp :: (a -> b) -> [a] -> [b]
mp _ [] = []
mp f (x:xs) = f x : mp f xs

flt :: (a -> Bool) -> [a] -> [a]
flt _ [] = []
flt p (x:xs)
  | p x = x : flt p xs
  | otherwise = flt p xs

qsortf :: (Ord a) => [a] -> [a]
qsortf [] = []
qsortf (x:xs) =
  let smallerSorted = qsortf (flt (<=x) xs)
      biggerSorted = qsortf (flt (>x) xs)
  in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => [a] -> a -> a
largestDivisible [] _ = error "empty list"
largestDivisible xs n = head (flt p (List.reverse (qsortf xs)))
  where p x = x `rem` n == 0

collatzChain :: (Integral a) => a -> [a]
collatzChain n
  | n <= 1 = [n]
  | odd n  = n : collatzChain (n*3 +1)
  | even n = n : collatzChain (n `div` 2)

longChains :: (Integral a) => a -> [[a]]
longChains n 
  | n <= 1 = error "too small"
  | n >  1 = filter p (map collatzChain [1..n])
  where p xs = length xs > 10

funs :: (Integral a) => [a -> a]
funs = map (*) [0..]



