module Pretty where
import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Jsonp

data Doc = ToBeDefined
           deriving (Show)

(<>) :: Doc -> Doc -> Doc
a <> b  = undefined

char :: Char -> Doc
char c = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

hcat :: [Doc] -> Doc
hcat xs = undefined

text :: String -> Doc
text str = undefined

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

double :: Double -> Doc
double num = undefined

simpleEscape :: [(Char, String)]
simpleEscape = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c = undefined

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

