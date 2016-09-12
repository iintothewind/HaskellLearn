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