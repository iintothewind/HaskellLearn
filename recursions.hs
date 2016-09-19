module Haskell.Recursions() where

wheremx :: (Ord a) => [a] -> a
wheremx [] = error "maximum of empty list"
wheremx [x] = x
wheremx (x:rs)
    | x > mxTail = x
    | otherwise = mxTail
    where mxTail = wheremx rs

recmx :: (Ord a) => [a] -> a
recmx [] = error "max of empty list"
recmx [x] = x
recmx (x:rs) = max x (recmx rs)

rptn :: Integer -> x -> [x]
rptn n x
    | n <= 0 = []
    | otherwise = x:rptn (n-1) x

tak :: Integral i => i -> [a] -> [a]
tak n  _ | n <=0        = []
tak _ []                = []
tak n (x:rs)            = x:tak (n-1) rs

rev :: [a] -> [a]
rev [] = []
rev (x:rs) = rev rs ++ [x]  -- ++ is not an efficient way, and its not stack safe either.