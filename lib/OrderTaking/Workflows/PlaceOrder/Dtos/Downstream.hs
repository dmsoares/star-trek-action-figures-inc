module OrderTaking.Workflows.PlaceOrder.Dtos.Downstream where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import Data.Text (Text)
import GHC.Generics (Generic)
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.OrderPlacedDto (OrderPlacedDto, mkOrderPlacedDto)
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.ShippableOrderPlacedDto (ShippableOrderPlacedDto, mkShippableOrderPlacedDto)
import OrderTaking.Workflows.PlaceOrder.Dtos.Instances ()
import OrderTaking.Workflows.PlaceOrder.Types.Events (PlaceOrderEvent (OrderPlacedEvent, ShippableOrderPlacedEvent))

type EventType = Text

data PlaceOrderEventDto
    = OrderPlacedEventDto OrderPlacedDto
    | ShippableOrderPlacedEventDto ShippableOrderPlacedDto
    deriving (Generic, Show)

-- | Convert a PlaceOrderEvent to a PlaceOrderEventDto
mkEventDto :: PlaceOrderEvent -> PlaceOrderEventDto
mkEventDto (OrderPlacedEvent eventData) = OrderPlacedEventDto (mkOrderPlacedDto eventData)
mkEventDto (ShippableOrderPlacedEvent eventData) = ShippableOrderPlacedEventDto (mkShippableOrderPlacedDto eventData)

instance ToJSON PlaceOrderEventDto where
    toEncoding (OrderPlacedEventDto eventData) = pairs ("eventType" .= ("OrderPlacedEvent" :: Text) <> "eventData" .= eventData)
    toEncoding (ShippableOrderPlacedEventDto eventData) = pairs ("eventType" .= ("ShippableOrderPlacedEvent" :: Text) <> "eventData" .= eventData)