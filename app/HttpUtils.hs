module HttpUtils where

import Network.HTTP.Types (status400)
import Web.Scotty (ActionM, json)
import Web.Scotty.Trans (status)

code400 :: String -> ActionM ()
code400 err = status status400 >> json err

malformedRequest :: ActionM ()
malformedRequest = code400 "Malformed request"
