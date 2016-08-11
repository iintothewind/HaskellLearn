module Haskell.WordCount(
countLines,
countWords,
countChars
) where  

countLines input = length (lines input) 

countWords input = length (words input)

countChars input = length input 

