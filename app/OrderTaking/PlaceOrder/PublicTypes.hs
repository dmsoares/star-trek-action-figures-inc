module OrderTaking.PlaceOrder.PublicTypes where

import Data.Text (Text)
import OrderTaking.Common.Order (ValidatedOrder)

data DomainEvent a = DomainEvent
    { eventType :: Text
    , eventData :: a
    }
    deriving (Show)

type OrderPlaced = DomainEvent ValidatedOrder