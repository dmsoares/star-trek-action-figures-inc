module OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.OrderPlacedDto where

import GHC.Generics (Generic)
import OrderTaking.Workflows.PlaceOrder.Types.Events.OrderPlaced qualified as OP

data OrderPlacedDto = OrderPlacedDto
    { orderId :: String
    , customerId :: String
    , orderLines :: [OrderLineDto]
    , totalPrice :: String
    }
    deriving (Generic, Show)

data OrderLineDto = OrderLineDto
    { productName :: String
    , quantity :: String
    , price :: String
    }
    deriving (Generic, Show)

mkOrderPlacedDto :: OP.OrderPlaced -> OrderPlacedDto
mkOrderPlacedDto event =
    OrderPlacedDto
        { orderId = show $ OP.orderId event
        , customerId = show $ OP.customerId event
        , orderLines = map createOrderPlacedLine $ OP.orderLines event
        , totalPrice = show $ OP.totalPrice event
        }
  where
    createOrderPlacedLine :: OP.OrderLine -> OrderLineDto
    createOrderPlacedLine line =
        OrderLineDto
            { productName = show $ OP.productName line
            , quantity = show $ OP.quantity line
            , price = show $ OP.price line
            }