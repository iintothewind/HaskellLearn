module Jsonp (
  JsonValue(..),
  getString,
  getInt,
  getDouble,
  getBool,
  getObject,
  getArray,
  isNull,
  renderJsonValue) where
import Data.List (intercalate)

data JsonValue = JsonString String
                |JsonNumber Double
                |JsonBool Bool
                |JsonNull
                |JsonObject [(String, JsonValue)]
                |JsonArray [JsonValue]
                 deriving (Eq, Ord, Show)

getString :: JsonValue -> Maybe String
getString (JsonString s) = Just s
getString _ = Nothing

getInt :: JsonValue -> Maybe Int
getInt (JsonNumber d) = Just (truncate d)
getInt _ = Nothing

getDouble :: JsonValue -> Maybe Double
getDouble (JsonNumber d) = Just d
getDouble _ = Nothing

getBool :: JsonValue -> Maybe Bool
getBool (JsonBool b) = Just b
getBool _ = Nothing

getObject :: JsonValue -> Maybe [(String, JsonValue)]
getObject (JsonObject o) = Just o
getObject _ = Nothing

getArray :: JsonValue -> Maybe [JsonValue]
getArray (JsonArray a) = Just a
getArray _ = Nothing

isNull :: JsonValue -> Bool
isNull j = j == JsonNull

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






