module OrderTaking.PlaceOrder.Dto where

import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import OrderTaking.Common.Order (UnvalidatedOrder (..), UnvalidatedOrderLine (..))

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

toUnvalidatedOrder :: OrderDto -> IO UnvalidatedOrder
toUnvalidatedOrder OrderDto{customerId, orderLines, shippingAddress} =
    nextRandom >>= \orderId ->
        return
            UnvalidatedOrder
                { orderId
                , customerId
                , orderLines = map toUnvalidatedOrderLine orderLines
                , shippingAddress
                }