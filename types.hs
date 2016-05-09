
--Function type declarations
removeNonUppercase :: String -> String 
removeNonUppercase st = [c|c<-st, c `elem` ['A'..'Z']]

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
