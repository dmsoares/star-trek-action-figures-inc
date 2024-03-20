module OrderTaking.Workflows.PlaceOrder.Dtos.Upstream where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder (
    UnvalidatedOrder (..),
    UnvalidatedOrderLine (..),
 )

data OrderDto = OrderDto
    { customerId :: Int
    , orderLines :: [OrderLineDto]
    , shippingAddress :: Text
    }
    deriving (Generic, Show)

data OrderLineDto = OrderLineDto
    { productName :: Text
    , quantity :: Int
    }
    deriving (Generic, Show)

toUnvalidatedOrderLine :: OrderLineDto -> UnvalidatedOrderLine
toUnvalidatedOrderLine OrderLineDto{productName, quantity} = UnvalidatedOrderLine{productName, quantity}

toUnvalidatedOrder :: UUID -> OrderDto -> UnvalidatedOrder
toUnvalidatedOrder orderId OrderDto{customerId, orderLines, shippingAddress} =
    UnvalidatedOrder
        { orderId
        , customerId
        , orderLines = map toUnvalidatedOrderLine orderLines
        , shippingAddress
        }
