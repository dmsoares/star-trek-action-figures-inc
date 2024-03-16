module OrderTaking.Workflows.PlaceOrder.Workflow.CreateEvents where

import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream (PlaceOrderEvent (..), mkOrderPlacedEvent)
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder (ValidatedOrder)

createEvents :: ValidatedOrder -> [PlaceOrderEvent]
createEvents validatedOrder =
    let orderPlaced = mkOrderPlacedEvent validatedOrder
     in [orderPlaced]