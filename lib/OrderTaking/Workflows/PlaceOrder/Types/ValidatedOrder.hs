module OrderTaking.Workflows.PlaceOrder.Types.ValidatedOrder where

import Data.UUID (UUID)
import OrderTaking.Common.Types (Address, ProductName, ProductQuantity)

-- | A validated order is an order that has been checked for correctness
data ValidatedOrder = ValidatedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [ValidatedOrderLine]
    , shippingAddress :: Address
    }
    deriving (Show)

data ValidatedOrderLine = ValidatedOrderLine
    { productName :: ProductName
    , quantity :: ProductQuantity
    }
    deriving (Show)
