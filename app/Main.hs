module Main where

import Data.Aeson (decode)
import Data.Text (Text)
import Network.HTTP.Types (status400)
import OrderTaking.Common.Instances ()
import OrderTaking.PlaceOrder.Dto (OrderDto, toUnvalidatedOrder)
import OrderTaking.PlaceOrder.Instances ()
import OrderTaking.PlaceOrder.Workflow (placeOrder)
import Persistence.DB (getProductCode, saveEvents)
import Web.Scotty (
    ActionM,
    body,
    get,
    html,
    json,
    liftIO,
    pathParam,
    post,
    scotty,
    status,
 )

main :: IO ()
main = scotty 3000
    $ do
        get "/scotty/:word" $ do
            beam <- pathParam "word"
            html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

        get "/worf/:word" $ do
            beam <- pathParam "word"
            html $ mconcat ["<h1>Worf, ", beam, " me up!</h1>"]

        post "/order" handlePostOrder

handlePostOrder :: ActionM ()
handlePostOrder = do
    maybeOrderDto <- decode <$> body :: ActionM (Maybe OrderDto)
    case maybeOrderDto of
        Nothing -> status status400 >> json ("Malformed request" :: Text)
        Just orderDto -> do
            unvalidatedOrder <- liftIO $ toUnvalidatedOrder orderDto
            result <- liftIO $ placeOrder getProductCode unvalidatedOrder
            case result of
                Left err -> status status400 >> json err
                Right validatedOrder -> do
                    liftIO $ saveEvents [validatedOrder]