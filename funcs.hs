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

drp :: Int -> [a] -> [a]
drp n xs = if n <= 0 || null xs
           then xs
           else drp (n-1) (tail xs)

--tak :: Int -> [a] -> [a]


