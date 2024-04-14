module HandlePlaceOrder where

import Data.Aeson.Decoding (decode)
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
import Web.Scotty (ActionM, body, json, liftIO)

-- | Handle the 'POST /order' endpoint
handlePlaceOrder :: ActionM ()
handlePlaceOrder = do
    -- try to deserialize the request body into an OrderDto
    maybeOrderDto <- deserializeOrderDto
    -- pattern match on the result:
    -- if it fails, return a malformed request
    -- otherwise, proceed with the workflow
    case maybeOrderDto of
        Nothing -> malformedRequest
        Just orderDto -> do
            (productMap, newOrderId) <- getDependencies

            --  PLACE-ORDER WORKFLOW
            --  - apply the 'placeOrder' workflow to the unvalidated order
            --  - note how all IO (getting dependencies, generating a random UUID) is done outside the workflow
            --    and the 'placeOrder' function is pure

            let unvalidatedOrder = toUnvalidatedOrder newOrderId orderDto
            let result = placeOrder productMap unvalidatedOrder

            -- 'result' is either an error message or a list of events
            -- either return a code 400 or persist the events
            case result of
                Left err -> code400 err
                Right events -> do
                    let eventDtos = createEventDtos events
                    pushEvents eventDtos >> json eventDtos
                    pushEvents eventDtos
                    -- return the events for demonstration purposes
                    json eventDtos

-- helper functions below this line:

-- try to deserialize incoming JSON into an OrderDto
deserializeOrderDto :: ActionM (Maybe OrderDto)
deserializeOrderDto = decode <$> body

-- 'placeOrder' workflow requires a ProductMap and a new UUID
-- and we need to perform some IO to get these dependencies
getDependencies :: ActionM (ProductMap, UUID)
getDependencies = do
    newOrderId <- liftIO nextRandom
    productMap <- liftIO getProductMap
    pure (productMap, newOrderId)

-- convert a list of PlaceOrderEvents into a list of PlaceOrderEventDtos
-- so we can push the events to the event store
createEventDtos :: [PlaceOrderEvent] -> [PlaceOrderEventDto]
createEventDtos = map mkEventDto

-- push event DTOs to the event store
pushEvents :: [PlaceOrderEventDto] -> ActionM ()
pushEvents = liftIO . saveEvents
