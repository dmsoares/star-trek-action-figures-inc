module Main where

import Data.Aeson (decode)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types (status400)
import OrderTaking.Common.Instances ()
import OrderTaking.Common.Order (ProductMap)
import OrderTaking.PlaceOrder.Dto (OrderDto, toUnvalidatedOrder)
import OrderTaking.PlaceOrder.Instances ()
import OrderTaking.PlaceOrder.Workflow (placeOrder)
import Persistence.DB (getProductMap, saveEvents)
import Web.Scotty (
    ActionM,
    body,
    json,
    liftIO,
    post,
    scotty,
    status,
 )

main :: IO ()
main = scotty 3000
    $ do
        post "/order" handleOrder

-- handleOrder :: ActionM ()
-- handleOrder = do
--     maybeOrderDto <- decodeOrderDto
--     flip (maybe malformedRequest) maybeOrderDto $ \orderDto -> do
--         (productMap, newOrderId) <- getDependencies
--         let unvalidatedOrder = toUnvalidatedOrder newOrderId orderDto
--         either code400 (liftIO . saveEvents) (placeOrder productMap unvalidatedOrder)

handleOrder :: ActionM ()
handleOrder =
    decodeOrderDto >>= \maybeOrderDto -> flip (maybe malformedRequest) maybeOrderDto $ \orderDto ->
        getDependencies >>= \(productMap, newOrderId) ->
            let unvalidatedOrder = toUnvalidatedOrder newOrderId orderDto
             in either code400 (liftIO . saveEvents) (placeOrder productMap unvalidatedOrder)

code400 :: String -> ActionM ()
code400 err = status status400 >> json err

malformedRequest :: ActionM ()
malformedRequest = code400 "Malformed request"

decodeOrderDto :: ActionM (Maybe OrderDto)
decodeOrderDto = decode <$> body :: ActionM (Maybe OrderDto)

getDependencies :: ActionM (ProductMap, UUID)
getDependencies = do
    productMap <- liftIO getProductMap
    newOrderId <- liftIO nextRandom
    pure (productMap, newOrderId)
