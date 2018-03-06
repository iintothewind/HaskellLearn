module Pretty ( 
  Doc(..),
  empty,char,text,double,line,softline,
  (<>),(</>),enclose,hcat,fsep,flatten,group,string, 
  simpleEscape,hexEscape,oneChar,smallHex,astral,series,punctuate,compact  
              ) where 

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

data Doc = Empty
          |Char Char
          |Text String
          |Line
          |Concat Doc Doc
          |Union Doc Doc
           deriving (Show)

empty :: Doc
empty = Empty 

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

softline :: Doc
softline = group line 

(<>) :: Doc -> Doc -> Doc
Empty <> d = d
d <> Empty = d
a <> b = Concat a b

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

concat :: [[a]] -> [a]
concat = foldr (++) []

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty 

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

flatten :: Doc -> Doc
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other  

group :: Doc -> Doc
group x = flatten x `Union` x

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

simpleEscape :: [(Char, String)]
simpleEscape = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

oneChar :: Char -> Doc
oneChar ch = case lookup ch simpleEscape of
              Just r -> text r
              Nothing | mustEscape ch -> hexEscape ch
                      | otherwise   -> char ch
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)


