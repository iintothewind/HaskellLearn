module Jsonp (
  JsonValue(..),
  getString,
  getInt,
  getDouble,
  getBool,
  getObject,
  getArray,
  isNull) where

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



















