{-# LANGUAGE ViewPatterns #-}
module Haskell.DataTypes() where
import Data.Maybe (fromMaybe)
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
            deriving (Show,Eq)
tree :: Tree
tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 3) 3 (Leaf 5))

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

-- type synonyms
type CustomerId = Int
type ReviewBody = String
data BookReview = Review BookInfo CustomerId ReviewBody

-- algebraic data types
-- actually its an enumeration
data Boolean = TRUE 
              |FALSE

data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

type Vector = (Double, Double)
data Shape = Circle Vector Double
            |Poly [Vector]
             deriving (Show)

data Customer = Customer {
  customerId :: Int,
  customerName :: String,
  customerAddress :: [String]
} deriving (Show)


customer1 = Customer 17413 "J.R. Hacker" ["255 Syntax Ct", "CA", "USA"]
customer2 = Customer {
              customerId = 271828,
              customerAddress = ["1048576 Disk Drive", "Milpitas, CA 95134", "USA"],
              customerName = "Jane Q. Citizen"
            }

data Mayb a = Jst a
              |None
               deriving (Show)

data List a = Conz a (List a)
             |Nil
              deriving (Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Conz x (fromList xs)
--fromList xs = foldr Conz Nil xs

toList :: List a -> [a]
toList Nil = []
toList (Conz x xs) = x:toList xs

data Tri a = Nood a (Tri a) (Tri a)
            |Empti
             deriving (Show)

lsnd :: [a] -> Maybe a
-- lsnd [] = Nothing
-- lsnd xs = if null (tail xs) then Nothing else Just (head (tail xs))

-- lsnd (_:x:_) = Just x
-- lsnd _ = Nothing

lsnd xs = case xs of (_:x:_) -> Just x
                     _ -> Nothing

unwrapp maybe defval = case maybe of { Nothing -> defval; Just val -> val }

def :: a -> Maybe a -> a
def = fromMaybe
-- use Maybe as a default parameter
multiProduct :: Int -> Maybe Int -> Maybe Int -> Maybe Int -> Int
multiProduct req1 (def 10 -> opt1) (def 20 -> opt2) (def 30 -> opt3)
  = req1 * opt1 * opt2 * opt3

-- nodesAreSame ltree rtree = case (ltree, rtree) of (Leaf a, Leaf b) -> if a == b then (Just (Leaf a)) else Nothing
--                                                  (Node a b c, Node d e f) -> if a == d && b == e && c == f then (Just (Node a b c)) else Nothing
--                                                  (_, _) -> Nothing

-- conditional evaluation with guards is better
nodesAreSame (Leaf a) (Leaf b)
  | a == b = Just (Leaf a)
nodesAreSame (Node a b c) (Node x y z)
  | a == x && b == y && c == z = Just (Node a b c)
nodesAreSame _ _ = Nothing

