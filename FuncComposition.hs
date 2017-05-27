module Haskell.FuncComposition() where
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

