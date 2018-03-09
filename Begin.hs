module Haskell.Begin() where
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmall x = if x > 100 then x else x * 2
conanO'Brien = "It's a-me, Conan O'Brien!"

--logic And, Or, Not
logicAnd = True && False
logicOr = False || True
logicNot = not (True && False)

--Strings
anyString= "test String"
headStr= head anyString

charCount :: Show a => a -> Int
charCount  = length . show 

wordCount :: String -> Int
wordCount = length . words 

lineCount :: String -> Int
lineCount = length . lines  

fib ::  Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)


