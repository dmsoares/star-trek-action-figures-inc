module OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder where

import Data.Text (Text)
import Data.UUID (UUID)

type UnvalidatedProductName = Text
type UnvalidatedAddress = Text
type UnvalidatedQuantity = Int

data UnvalidatedOrder = UnvalidatedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [UnvalidatedOrderLine]
    , shippingAddress :: UnvalidatedAddress
    }
    deriving (Show)

data UnvalidatedOrderLine = UnvalidatedOrderLine
    { productName :: UnvalidatedProductName
    , quantity :: UnvalidatedQuantity
    }
    deriving (Show)
