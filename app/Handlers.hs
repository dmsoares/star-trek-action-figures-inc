module Handlers where

import Data.Aeson (ToJSON)
import Data.Aeson.Decoding (decode)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import HttpUtils (code400, malformedRequest)
import Infrastructure.DB (getProductMap, saveEvents)
import OrderTaking.Workflows.PlaceOrder.Dtos.Instances ()
import OrderTaking.Workflows.PlaceOrder.Dtos.Upstream (Order, toUnvalidatedOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow (placeOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder (ProductMap)
import Web.Scotty (ActionM, body)
import Web.Scotty.Trans (liftIO)

handlePlaceOrder :: ActionM ()
handlePlaceOrder = do
    -- try to deserialize the request
    maybeOrderDto <- deserializeOrderDto
    case maybeOrderDto of
        -- if the deserialization fails, return a 400
        Nothing -> malformedRequest
        -- otherwise, continue
        Just orderDto -> do
            -- get the workflow dependencies
            (productMap, newOrderId) <- getDependencies
            -- run the workflow
            persistEvents $ placeOrder productMap (toUnvalidatedOrder newOrderId orderDto)

deserializeOrderDto :: ActionM (Maybe Order)
deserializeOrderDto = decode <$> body :: ActionM (Maybe Order)

persistEvents :: (ToJSON a) => Either String [a] -> ActionM ()
persistEvents workflowResult = case workflowResult of
    Left err -> code400 err
    Right events -> liftIO $ saveEvents events

getDependencies :: ActionM (ProductMap, UUID)
getDependencies =
    liftIO getProductMap >>= \productMap ->
        liftIO nextRandom >>= \newOrderId -> return (productMap, newOrderId)