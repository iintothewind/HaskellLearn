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

