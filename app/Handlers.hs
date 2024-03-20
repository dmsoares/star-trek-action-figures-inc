module Handlers where

import Control.Exception (Exception)
import Data.Aeson (ToJSON)
import Data.Aeson.Decoding (decode)
import Data.Data (Typeable)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import HttpUtils (code400, malformedRequest)
import Infrastructure.DB (getProductMap, saveEvents)
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream (PlaceOrderEventDto, mkEventDto)
import OrderTaking.Workflows.PlaceOrder.Dtos.Instances ()
import OrderTaking.Workflows.PlaceOrder.Dtos.Upstream (OrderDto, toUnvalidatedOrder)
import OrderTaking.Workflows.PlaceOrder.Types.Events (PlaceOrderEvent)
import OrderTaking.Workflows.PlaceOrder.Workflow (placeOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.PriceOrder (ProductMap)
import Web.Scotty (ActionM, body, throw)
import Web.Scotty.Trans (liftIO)

handlePlaceOrder :: ActionM ()
handlePlaceOrder = do
    -- try to deserialize the request
    maybeOrderDto <- deserializeOrderDto
    flip (maybe malformedRequest) maybeOrderDto $ \orderDto -> do
        -- get the workflow dependencies
        (productMap, newOrderId) <- getDependencies
        -- run the workflow
        let result = placeOrder productMap (toUnvalidatedOrder newOrderId orderDto)
        -- persist the events or raise code 400
        either code400 (persistEvents . createEventDtos) result

deserializeOrderDto :: ActionM (Maybe OrderDto)
deserializeOrderDto = decode <$> body :: ActionM (Maybe OrderDto)

createEventDtos :: [PlaceOrderEvent] -> [PlaceOrderEventDto]
createEventDtos = fmap mkEventDto

persistEvents :: (ToJSON a) => [a] -> ActionM ()
persistEvents = liftIO . saveEvents

getDependencies :: ActionM (ProductMap, UUID)
getDependencies = do
    newOrderId <- liftIO nextRandom
    maybeProductMap <- liftIO getProductMap
    case maybeProductMap of
        Nothing -> throw $ Error500 "Could not load product map"
        Just productMap -> pure (productMap, newOrderId)

data HttpError = Error400 String | Error500 String
    deriving (Show, Typeable)

instance Exception HttpError
