module Main (main) where
import Jsonp

main = print (JsonObject [("foo", JsonNumber 1), ("bar", JsonBool False)])


