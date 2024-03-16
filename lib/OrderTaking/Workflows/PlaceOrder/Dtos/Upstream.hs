module OrderTaking.Workflows.PlaceOrder.Dtos.Upstream where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder (
    UnvalidatedOrder (..),
    UnvalidatedOrderLine (..),
 )

data Order = Order
    { customerId :: Int
    , orderLines :: [OrderLine]
    , shippingAddress :: Text
    }
    deriving (Generic, Show)

data OrderLine = OrderLine
    { productName :: Text
    , quantity :: Int
    }
    deriving (Generic, Show)

toUnvalidatedOrderLine :: OrderLine -> UnvalidatedOrderLine
toUnvalidatedOrderLine OrderLine{productName, quantity} = UnvalidatedOrderLine{productName, quantity}

toUnvalidatedOrder :: UUID -> Order -> UnvalidatedOrder
toUnvalidatedOrder orderId Order{customerId, orderLines, shippingAddress} =
    UnvalidatedOrder
        { orderId
        , customerId
        , orderLines = map toUnvalidatedOrderLine orderLines
        , shippingAddress
        }
