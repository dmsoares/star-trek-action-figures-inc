module OrderTaking.Workflows.PlaceOrder.Types.PricedOrder where

import Data.UUID (UUID)
import OrderTaking.Common.Types (Address, Price, ProductName, ProductQuantity)

-- | A priced order is an order that has been priced
data PricedOrder = PricedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [PricedOrderLine]
    , shippingAddress :: Address
    , totalPrice :: Price
    }
    deriving (Show)

data PricedOrderLine = PricedOrderLine
    { productName :: ProductName
    , quantity :: ProductQuantity
    , price :: Price
    }
    deriving (Show)
