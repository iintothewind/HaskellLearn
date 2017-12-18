module Haskell.Lets() where
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

wooBar :: [String]
wooBar = [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]

squares :: (Num a) => [a]
squares = let square x = x * x in [square 5, square 4, square 3]

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

lend :: (Num a, Ord a) => a -> a -> Maybe a
lend amount budget = let reserve = 100 
                         balance = budget - amount
                     in if balance < reserve then Nothing else Just balance

lendMore :: (Fractional a, Ord a) => a -> a -> Maybe a
lendMore amount budget = if balance < (reserve * 0.5) then Nothing else Just balance
                         where reserve = 100
                               balance = budget - amount

pluralise :: String -> [Int] -> [String]
pluralise word = map plural where plural 0 = "no " ++ word ++ "s"
                                  plural 1 = "one " ++ word
                                  plural n = show n ++ " " ++ word ++ "s"



