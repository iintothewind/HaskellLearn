module Haskell.DataTypes() where
--enumerations
data Thing = Shoe
            |Ship
            |SealingWax
            |Cabbage
            |King
             deriving Show

shoe :: Thing
shoe = Shoe

lstOfThings :: [Thing]
lstOfThings = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isBig :: Thing -> Bool
isBig Ship = True
isBig King = True
isBig _    = False

--beyond enumerations
data FailableDouble  = Failure
                      |Ok Double
                       deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = Ok (x / y)

failureToZero :: FailableDouble -> Double
failuretozero Failure = 0
failureToZero (Ok d)  = d

data Person = Person String Int Thing
              deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getName :: Person -> String
getName (Person n _ _) = "The name of this Person is: " ++ n

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

getAge :: Person -> String
getAge (Person _ a _) = "The age of this Person is: " ++ show a

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", your favorite is SealingWax"
checkFav (Person n _ _)          = n ++ ", your favourite thing is lame"

-- algebraic data types in general

-- pattern matching

-- case expressions
descStr :: String -> String
descStr s = case s of
              []      -> "empty string"
              ('H':_) -> "a string starts with H"
              _       -> "other case"

failToZero :: FailableDouble -> Double
failToZero x = case x of
                 Failure -> 0
                 Ok d    -> d

data IntList = Empty
              |Cons Int IntList
               deriving Show

intListPrd :: IntList -> Int
intListPrd Empty       = 1
intListPrd (Cons x rs) = x * intListPrd rs

hdIntList :: IntList -> Int
hdIntList xs = case xs of
                 Empty      -> error "head of empty list"
                 (Cons x _) -> x

data Tree = Leaf Int
           |Node Tree Int Tree
            deriving Show
tree :: Tree
tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 3) 3 (Leaf 5))

