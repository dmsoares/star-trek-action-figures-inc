module OrderTaking.Workflows.PlaceOrder.Types.Events.OrderPlaced where

import Data.UUID (UUID)
import OrderTaking.Common.Types (Price, ProductName, ProductQuantity)
import OrderTaking.Workflows.PlaceOrder.Types.PricedOrder qualified as PO

data OrderPlaced = OrderPlaced
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [OrderLine]
    , totalPrice :: Price
    }
    deriving (Show)

data OrderLine = OrderLine
    { productName :: ProductName
    , quantity :: ProductQuantity
    , price :: Price
    }
    deriving (Show)

mkOrderPlaced :: PO.PricedOrder -> OrderPlaced
mkOrderPlaced order =
    OrderPlaced
        (PO.orderId order)
        (PO.customerId order)
        (map createOrderPlacedLine $ PO.orderLines order)
        (PO.totalPrice order)
  where
    createOrderPlacedLine :: PO.PricedOrderLine -> OrderLine
    createOrderPlacedLine line =
        OrderLine
            (PO.productName line)
            (PO.quantity line)
            (PO.price line)