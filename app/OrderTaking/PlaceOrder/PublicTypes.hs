module OrderTaking.PlaceOrder.PublicTypes where

import Data.Text (Text)
import GHC.Generics (Generic)
import OrderTaking.Common.Order (ValidatedOrder)

data DomainEvent a = DomainEvent
    { eventType :: Text
    , eventData :: a
    }
    deriving (Generic, Show)

type OrderPlaced = DomainEvent ValidatedOrder
