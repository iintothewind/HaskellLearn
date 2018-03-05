module PutJson where
import Jsonp(JsonValue(..), renderJsonValue)

putJsonValue :: JsonValue -> IO ()
putJsonValue v = putStrLn (renderJsonValue v)














