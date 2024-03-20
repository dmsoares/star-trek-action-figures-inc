module OrderTaking.Workflows.PlaceOrder.Types.Events where

import OrderTaking.Common.Event (Event (Event))
import OrderTaking.Workflows.PlaceOrder.Types.Events.OrderPlaced (OrderPlaced, mkOrderPlaced)
import OrderTaking.Workflows.PlaceOrder.Types.Events.ShippableOrderPlaced (ShippableOrderPlaced, mkShippableOrderPlaced)
import OrderTaking.Workflows.PlaceOrder.Types.PricedOrder (PricedOrder)

data PlaceOrderEvent
    = OrderPlacedEvent (Event OrderPlaced)
    | ShippableOrderPlacedEvent (Event ShippableOrderPlaced)
    deriving (Show)

mkOrderPlacedEvent :: PricedOrder -> PlaceOrderEvent
mkOrderPlacedEvent = OrderPlacedEvent . Event "OrderPlaced" . mkOrderPlaced

mkShippableOrderPlacedEvent :: PricedOrder -> PlaceOrderEvent
mkShippableOrderPlacedEvent = ShippableOrderPlacedEvent . Event "ShippableOrderPlaced" . mkShippableOrderPlaced
