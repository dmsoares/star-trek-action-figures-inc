module OrderTaking.PlaceOrder.PublicTypes where

import Data.Text (Text)
import GHC.Generics (Generic)
import OrderTaking.Common.Order (ValidatedOrder)

data Event a = Event
    { eventType :: Text
    , eventData :: a
    }
    deriving (Generic, Show)

newtype DomainEvent = OrderPlaced (Event ValidatedOrder)
    deriving (Generic, Show)

createOrderPlacedEvent :: ValidatedOrder -> DomainEvent
createOrderPlacedEvent = OrderPlaced . Event "OrderPlaced"