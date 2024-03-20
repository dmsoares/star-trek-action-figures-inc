module Handlers where

import Control.Exception (Exception)
import Data.Aeson (ToJSON)
import Data.Aeson.Decoding (decode)
import Data.Data (Typeable)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import HttpUtils (code400, malformedRequest)
import Infrastructure.DB (getProductMap, saveEvents)
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream (mkEventDto)
import OrderTaking.Workflows.PlaceOrder.Dtos.Instances ()
import OrderTaking.Workflows.PlaceOrder.Dtos.Upstream (OrderDto, toUnvalidatedOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow (placeOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.PriceOrder (ProductMap)
import Web.Scotty (ActionM, body, throw)
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
            persistEvents . fmap (fmap mkEventDto) $ placeOrder productMap (toUnvalidatedOrder newOrderId orderDto)

deserializeOrderDto :: ActionM (Maybe OrderDto)
deserializeOrderDto = decode <$> body :: ActionM (Maybe OrderDto)

persistEvents :: (ToJSON a) => Either String [a] -> ActionM ()
persistEvents workflowResult = case workflowResult of
    Left err -> code400 err
    Right events -> liftIO $ saveEvents events

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
