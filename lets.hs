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

