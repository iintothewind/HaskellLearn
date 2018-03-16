module Main (main) where
import           Jsonp (JsonValue (..))

main = print (JsonObject [("foo", JsonNumber 1), ("bar", JsonBool False)])


