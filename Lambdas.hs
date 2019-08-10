module Haskell.Lamdas () where

collatzChain :: (Integral a) => a -> [a]
collatzChain n
  | n <= 1 = [n]
  | odd n  = n : collatzChain (n*3 +1)
  | otherwise  = n : collatzChain (n `div` 2)

lambdaLongChains :: (Integral a) => a -> [[a]]
lambdaLongChains n
  | n <= 1 = error "too small"
  | otherwise  = filter (\xs -> length xs > 10) (map collatzChain [1..n])

sumThree :: (Num a) => a -> a -> a -> a
sumThree = \x y z -> x + y + z

flp :: (a -> b -> c) -> b -> a -> c
flp f = \a b -> f b a

zw :: (Integral a) => [a]
zw = zipWith (\a b -> a*b) [1,2,3,4,5] [5,4,3,2,1]

mp :: (Integral a) => [a]
mp = map (\(a,b) -> a + b) [(1,2),(3,4),(5,6),(7,8)]



