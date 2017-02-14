module Haskell.Lamdas () where

collatzChain :: (Integral a) => a -> [a]
collatzChain n
  | n <= 1 = [n]
  | odd n  = n : collatzChain (n*3 +1)
  | even n = n : collatzChain (n `div` 2)

lambdaLongChains :: (Integral a) => a -> [[a]]
lambdaLongChains n
  | n <= 1 = error "too small"
  | n >  1 = filter (\xs -> length xs > 10) (map collatzChain [1..n])

