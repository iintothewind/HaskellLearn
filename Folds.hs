module Haskell.Folds () where
import           Data.Char     (digitToInt, isDigit)
import           Data.Foldable (foldl')
-- foldl :: (b -> a -> b) -> b -> t a -> b
-- foldr :: (a -> b -> b) -> b -> t a -> b
{-
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)

foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))
-}

suml :: (Num a) => [a] -> a
suml = foldl (\acc x -> acc + x) 0

eleml :: (Eq a) => a -> [a] -> Bool
eleml x = foldl (\acc y -> (y==x || acc)) False 

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = reverse (foldl (\acc x -> f x : acc) [] xs)

mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\x acc -> f x : acc) [] 
maxl :: (Ord a) => [a] -> a
maxl = foldl1 (\x acc -> if x > acc then x else acc)

revl :: [a] -> [a]
-- revl = foldl (\acc x -> x:acc) []
revl = foldl (flip (:)) []

prd :: (Num a) => [a] -> a
prd = foldr1 (*)

had :: [a] -> a
had = foldr1 (\x _ -> x)

lst :: [a] -> a
lst = foldl1 (\_ x -> x)

toInt :: String -> Int
toInt ('-':rs) = - (toInt rs)
toInt ('+':rs) = toInt rs
toInt xs       = foldl' (\z c -> z * 10 + digitToInt c) 0 xs

neg :: Either String Int -> Either String Int
neg (Left s)  = Left s
neg (Right n) = Right (-n)

tie :: String -> Either String Int
tie [] = Left "no input"
tie ('+':rs) = tie rs
tie ('-':rs) = neg (tie rs)
tie xs = foldl' cal (Right 0) xs where
  cal s@(Left _) _ = s
  cal (Right z) c
    | isDigit c = Right (z * 10 + digitToInt c)
    | otherwise = Left (c : " is not a digit")

-- Data.Foldable.concat
cnct :: [[a]] -> [a]
cnct = foldr (++) []

-- GHC.List.takeWhile
tkw :: (a -> Bool) -> [a] -> [a]
tkw p = foldr (\a b -> if p a then a : b else []) []

tkwr :: (a -> Bool) -> [a] -> [a]
tkwr _ [] = []
tkwr p (x:rs)
  | p x = x : tkwr p rs
  | not (p x) = []

-- Data.List.groupBy
gpby :: (a -> a -> Bool) -> [a] -> [[a]]
gpby _ [] = []
gpby p xs = foldr (\a z -> if p a (head (head z)) then (a:head z):tail z else [a]:z) [[last xs]] (init xs)

-- Data.Foldable.any
ny :: (a -> Bool) -> [a] -> Bool
ny p = foldr (\a z -> z || p a) False

-- GHC.List.cycle
cle :: [a] -> [a]
cle xs = foldr (:) (cle xs) xs

-- Data.OldList.words
wds :: String -> [String]
wds = foldr brk [] where
  brk c z
    | null z = [[c]]
    | c == ' ' = [] : z
    | otherwise = (c : head z) : tail z

-- Data.OldList.unlines
ulns :: [String] -> String
ulns = foldr (\w z -> w ++ "\n" ++ z) []


-- Data.Foldable.foldl'
fdl' _    zero []     = zero
fdl' step zero (x:xs) =
    let new = step zero x
    in  new `seq` fdl' step new xs



