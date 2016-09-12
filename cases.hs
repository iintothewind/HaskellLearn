module Haskell.Cases() where

hd :: [a] -> a
hd [] = error "No head for empty lists!"
hd (x:_) = x

caseDescLst :: [a] -> String
caseDescLst xs = "The list is " ++ case xs of [] -> "empty."
                                              [x] -> "a singleton list."
                                              _ -> "a longer list."

whereDescLst :: [a] -> String
whereDescLst xs = "The list is " ++ desc xs
    where desc [] = "empty."
          desc [x] = "a singleton list."
          desc _ = "a longer list."
