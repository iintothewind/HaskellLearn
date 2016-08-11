module Haskell.Funcs() where 
fadd :: Int -> Int -> Int -> Int
fadd x y z = x + y + z

sumtorial :: Integer -> Integer
sumtorial n = if n <= 0 then 0 else n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 1
  | "Haskell" > "c++" = 3
  | otherwise         = 4
foo n
  | n < 0             = 0
  | n `mod` 17 == 2   = -43
  | otherwise         = n + 3

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0  = True
  | otherwise       = False

isBetterEven :: Integer -> Bool
isBetterEven = even

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []          = []
sumEveryTwo [x]         = [x]
sumEveryTwo (x:(y:zs))  = (x+y) : sumEveryTwo zs

lts = ['A'..'Z']++['a'..'z']
isLetters :: String -> Bool
isLetters []            = False
isLetters [x]           = x `elem` lts
isLetters (x:rs)        = (x `elem` lts) && isLetters rs

drp :: Integral i => i -> [a] -> [a]
drp _ []                = []
drp n xs | n <= 0       = xs
drp n (x:rs)            = drp (n-1) rs

tak :: Integral i => i -> [a] -> [a]
tak n  _ | n <=0        = []
tak _ []                = []
tak n (x:rs)            = x:tak (n-1) rs

