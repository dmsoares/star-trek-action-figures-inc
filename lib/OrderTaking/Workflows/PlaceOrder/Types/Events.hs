module OrderTaking.Workflows.PlaceOrder.Types.Events where

import OrderTaking.Workflows.PlaceOrder.Types.Events.OrderPlaced (OrderPlaced, mkOrderPlaced)
import OrderTaking.Workflows.PlaceOrder.Types.Events.ShippableOrderPlaced (ShippableOrderPlaced, mkShippableOrderPlaced)
import OrderTaking.Workflows.PlaceOrder.Types.PricedOrder (PricedOrder)

data PlaceOrderEvent
    = OrderPlacedEvent OrderPlaced
    | ShippableOrderPlacedEvent ShippableOrderPlaced
    deriving (Show)

mkOrderPlacedEvent :: PricedOrder -> PlaceOrderEvent
mkOrderPlacedEvent = OrderPlacedEvent . mkOrderPlaced

mkShippableOrderPlacedEvent :: PricedOrder -> PlaceOrderEvent
mkShippableOrderPlacedEvent = ShippableOrderPlacedEvent . mkShippableOrderPlaced
