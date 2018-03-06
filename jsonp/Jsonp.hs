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
import Pretty (double,string,text,series,(<>),compact) 

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

renderJsonValue (JsonString s) = string s
renderJsonValue (JsonNumber n) = double n
renderJsonValue (JsonBool True) = string "True"
renderJsonValue (JsonBool False) = string "False"
renderJsonValue JsonNull = string "null"

renderJsonValue (JsonArray a) = series '[' ']' renderJsonValue a

renderJsonValue (JsonObject o) = series '{' '}' field o
  where field (name, v) = string name <> text ": " <> renderJsonValue v 








