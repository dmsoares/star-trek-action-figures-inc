module HttpUtils where

import Data.Text (Text)
import Network.HTTP.Types (Status, status400, status500)
import Web.Scotty (ActionM, json)
import Web.Scotty.Trans (status)

errorWithCode :: Status -> Text -> ActionM ()
errorWithCode code err = status code >> json err

code400 :: Text -> ActionM ()
code400 = errorWithCode status400

code500 :: Text -> ActionM ()
code500 = errorWithCode status500

malformedRequest :: ActionM ()
malformedRequest = code400 "Malformed request"
