module OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.ShippableOrderPlacedDto where

import GHC.Generics (Generic)
import OrderTaking.Workflows.PlaceOrder.Types.Events.ShippableOrderPlaced qualified as SOP

data ShippableOrderPlacedDto = ShippableOrderPlacedDto
    { orderId :: String
    , customerId :: String
    , orderLines :: [OrderLineDto]
    , shippingAddress :: String
    }
    deriving (Generic, Show)

data OrderLineDto = OrderLineDto
    { productName :: String
    , quantity :: String
    }
    deriving (Generic, Show)

mkShippableOrderPlacedDto :: SOP.ShippableOrderPlaced -> ShippableOrderPlacedDto
mkShippableOrderPlacedDto event =
    ShippableOrderPlacedDto
        (show $ SOP.orderId event)
        (show $ SOP.customerId event)
        (map createOrderPlacedLine $ SOP.orderLines event)
        (show $ SOP.shippingAddress event)
  where
    createOrderPlacedLine :: SOP.OrderLine -> OrderLineDto
    createOrderPlacedLine line =
        OrderLineDto
            (show $ SOP.productName line)
            (show $ SOP.quantity line)