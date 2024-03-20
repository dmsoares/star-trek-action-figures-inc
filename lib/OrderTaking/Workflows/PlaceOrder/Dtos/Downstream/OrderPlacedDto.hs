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
        (show $ OP.orderId event)
        (show $ OP.customerId event)
        (map createOrderPlacedLine $ OP.orderLines event)
        (show $ OP.totalPrice event)
  where
    createOrderPlacedLine :: OP.OrderLine -> OrderLineDto
    createOrderPlacedLine line =
        OrderLineDto
            (show $ OP.productName line)
            (show $ OP.quantity line)
            (show $ OP.price line)