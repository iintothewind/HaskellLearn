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

--Function type declarations
removeNonUppercase :: String -> String 
removeNonUppercase st = [c | c<-st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

letters :: String -> Bool
letters s = null [c | c<-s, c `notElem` (['A'..'Z']++['a'..'z'])]

--Arithmetic
--error on 2.1 + length [1,2,3], because length returns an Int, not Float
anyFloat :: Float
anyFloat = 2.1 + fromIntegral (length [1,2,3]) 

--error on 12/5, because / returns a Fractional
anyInt :: Int
anyInt = 12 `div` 5

-- similar implementation of lines in prelude
spLines :: String -> [String]
spLines chars = case break (=='\n') chars of (left, right)
                                                  | null right || right == "" || right == "\n"  -> [left]
                                                  | otherwise -> left : splitLines (tail right) 

isLineTerminator c = c == '\r' || c == '\n'
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []









