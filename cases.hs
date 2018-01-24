module Haskell.Cases() where

hd :: [a] -> a
hd []    = error "No head for empty lists!"
hd (x:_) = x

caseDescLst :: [a] -> String
caseDescLst xs = "The list is " ++ case xs of []  -> "empty."
                                              [x] -> "a singleton list."
                                              _   -> "a longer list."

whereDescLst :: [a] -> String
whereDescLst xs = "The list is " ++ desc xs
  where desc (_:x:_) = "a longer list."
        desc [x] = "a singleton list."
        desc _  = "empty."

-- whereDescLst xs = "The list is " ++ desc xs where { desc []  = "empty."; desc [x] = "a singleton list."; desc _   = "a longer list." }

lsnd :: [a] -> Maybe a
-- lsnd [] = Nothing
-- lsnd xs = if null (tail xs) then Nothing else Just (head (tail xs))

-- lsnd (_:x:_) = Just x
-- lsnd _ = Nothing

lsnd xs = case xs of (_:x:_) -> Just x
                     _ -> Nothing

foo a = case round a `mod` 2 of { 0 -> "even"; _ -> "odd" }

