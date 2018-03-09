module Haskell.Composition() where
--function composition
-- . is defined as
-- (.) :: (b -> c) -> (a -> b) -> a -> c

negateAbs :: (Num a) => [a] -> [a]
negateAbs xs = map (\x -> negate (abs x)) xs

negateAbsd :: (Num a) => [a] -> [a]
negateAbsd xs = map (negate.abs) xs

ngAbsPtFree :: (Num a) => [a] -> [a]
ngAbsPtFree = map $ negate.abs

oddSquareSum :: Int
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

ossd :: Integral a => [a] -> a
ossd = sum.takeWhile (<10000).filter odd.map (^2)

ossa :: Integral a => [a] -> a
ossa =
  let oddSquares = filter odd.map (^2)
      belowLimit = takeWhile (<10000).oddSquares
  in  sum.belowLimit

-- Function application with $
-- $ is defined as:
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x

sumMap :: (Floating a) => [a] -> a
sumMap xs = sum (map sqrt xs)

sumMapd :: (Floating a) => [a] -> a
sumMapd xs = sum $ map sqrt xs

-- sqrt 3+4+9 compilation error
-- sqrt (3+4+9)
-- sqrt $ 3+4+9

sumFlt :: (Num a, Ord a) => [a] -> a
sumFlt xs = sum (filter (>10) (map (*2) xs))

sumFltd :: (Num a, Ord a) => [a] -> a
sumFltd xs = sum $ filter (>10) $ map (*2) xs

--But apart from getting rid of parentheses, $ means that function application can be treated just like another function. 
--That way, we can, for instance, map function application over a list of functions.
xsd = map ($ 3) [(4+), (10*), (^2), sqrt]  




