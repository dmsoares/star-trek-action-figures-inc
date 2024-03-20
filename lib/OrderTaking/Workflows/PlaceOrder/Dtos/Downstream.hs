module OrderTaking.Workflows.PlaceOrder.Dtos.Downstream where

import GHC.Generics (Generic)
import OrderTaking.Common.Event (Event (..))
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.OrderPlacedDto (OrderPlacedDto, mkOrderPlacedDto)
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.ShippableOrderPlacedDto (ShippableOrderPlacedDto, mkShippableOrderPlacedDto)
import OrderTaking.Workflows.PlaceOrder.Types.Events (PlaceOrderEvent (OrderPlacedEvent, ShippableOrderPlacedEvent))

data PlaceOrderEventDto = OrderPlaceEventDto OrderPlacedDto | ShippableOrderPlacedEventDto ShippableOrderPlacedDto
    deriving (Generic, Show)

mkEventDto :: PlaceOrderEvent -> PlaceOrderEventDto
mkEventDto (OrderPlacedEvent (Event _ event)) = OrderPlaceEventDto $ mkOrderPlacedDto event
mkEventDto (ShippableOrderPlacedEvent (Event _ event)) = ShippableOrderPlacedEventDto $ mkShippableOrderPlacedDto event