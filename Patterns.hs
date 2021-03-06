module Haskell.Patterns() where
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Sevent"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addPairs :: (Num a) => (a,a) -> (a,a) -> (a,a)
addPairs (x1,y1) (x2,y2) = (x1+x2, y1+y2)

first :: (a, b, c) -> a
first  (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

hd :: [a] -> a
hd []    = error "Can't call head on an empty list, dummy"
hd (x:_) = x

tl :: [a] -> [a]
tl []     = []
tl (x:rs) = rs

len :: (Integral n) => [a] -> n
len []     = 0
len (_:rs) = 1 + len rs

sumup :: (Num a) => [a] -> a
sumup []     = 0
sumup (x:rs) = x + sumup rs

bigger :: (Ord a) => a -> a -> a
bigger a b
  | a > b = a
  | otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight"
  | bmi <= normal = "You're supposedly normal"
  | bmi <= fat = "You're fat"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

mx :: (Ord a) => a -> a -> a
mx a b
  | a > b = a
  | otherwise = b

cmp :: (Ord a) => a -> a -> Ordering
a `cmp` b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

fib1 :: Int -> Int
fib1 = (map fib' [0 ..] !!)
  where
    fib' n
      | n < 2 = n
      | otherwise = fib1 (n - 1) + fib1 (n - 2) 

-- eta-expansion, perf drop 
fib2 :: Int -> Int
fib2 x = map fib' [0 ..] !! x
  where
    fib' n
      | n < 2 = n
      | otherwise = fib1 (n - 1) + fib1 (n - 2) 

-- the o(n) solution
fib :: Int -> Int
fib a = fibt a 0 1
  where
    fibt n x y 
      | n == 0 = x
      | otherwise = fibt (n-1) y (x+y)

data Tree = Leaf Int
           |Node Tree Int Tree
            deriving (Show,Eq)
-- conditional evaluation with guards is better
nodesAreSame (Leaf a) (Leaf b)
  | a == b = Just (Leaf a)
nodesAreSame (Node a b c) (Node x y z)
  | a == x && b == y && c == z = Just (Node a b c)
nodesAreSame _ _ = Nothing
