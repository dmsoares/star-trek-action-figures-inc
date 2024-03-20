module OrderTaking.Workflows.PlaceOrder.Types.Events.ShippableOrderPlaced where

import Data.UUID (UUID)
import OrderTaking.Common.Types (Address, ProductName, ProductQuantity)
import OrderTaking.Workflows.PlaceOrder.Types.PricedOrder qualified as PO

data ShippableOrderPlaced = ShippableOrderPlaced
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [OrderLine]
    , shippingAddress :: Address
    }
    deriving (Show)

data OrderLine = OrderLine
    { productName :: ProductName
    , quantity :: ProductQuantity
    }
    deriving (Show)

mkShippableOrderPlaced :: PO.PricedOrder -> ShippableOrderPlaced
mkShippableOrderPlaced order =
    ShippableOrderPlaced
        (PO.orderId order)
        (PO.customerId order)
        (map createOrderPlacedLine $ PO.orderLines order)
        (PO.shippingAddress order)
  where
    createOrderPlacedLine :: PO.PricedOrderLine -> OrderLine
    createOrderPlacedLine line =
        OrderLine
            (PO.productName line)
            (PO.quantity line)