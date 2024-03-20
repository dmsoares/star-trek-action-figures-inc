module OrderTaking.Workflows.PlaceOrder.Workflow.CreateEvents where

import OrderTaking.Workflows.PlaceOrder.Types.Events (PlaceOrderEvent, mkOrderPlacedEvent, mkShippableOrderPlacedEvent)
import OrderTaking.Workflows.PlaceOrder.Types.PricedOrder (PricedOrder)

createEvents :: PricedOrder -> [PlaceOrderEvent]
createEvents pricedOrder =
    let orderPlaced = mkOrderPlacedEvent pricedOrder
        shippableOrderPlaced = mkShippableOrderPlacedEvent pricedOrder
     in [orderPlaced, shippableOrderPlaced]