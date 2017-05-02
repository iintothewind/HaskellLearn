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


