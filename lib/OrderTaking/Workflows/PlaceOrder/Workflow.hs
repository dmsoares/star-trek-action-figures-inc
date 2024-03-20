module OrderTaking.Workflows.PlaceOrder.Workflow where

import Control.Monad ((>=>))
import OrderTaking.Workflows.PlaceOrder.Types.Events (PlaceOrderEvent)
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder (UnvalidatedOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.CreateEvents (createEvents)
import OrderTaking.Workflows.PlaceOrder.Workflow.PriceOrder (ProductMap, priceOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder (validateOrder)

placeOrder :: ProductMap -> UnvalidatedOrder -> Either String [PlaceOrderEvent]
placeOrder productMap =
    validateOrder
        >=> priceOrder productMap
        >=> pure . createEvents
