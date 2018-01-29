module PutJson where
import Data.List (intercalate)
import Jsonp

renderJsonValue :: JsonValue -> String
renderJsonValue (JsonString s) = show s
renderJsonValue (JsonNumber n) = show n
renderJsonValue (JsonBool True) = "True"
renderJsonValue (JsonBool False) = "False"
renderJsonValue JsonNull = "null"

renderJsonValue (JsonObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ renderJsonValue v

renderJsonValue (JsonArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJsonValue vs)

putJsonValue :: JsonValue -> IO ()
putJsonValue v = putStrLn (renderJsonValue v)














