module OrderTaking.Workflows.PlaceOrder.Workflow where

import Control.Monad ((>=>))
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream (PlaceOrderEvent)
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder (UnvalidatedOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.CreateEvents (createEvents)
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder (ProductMap, validateOrder)

placeOrder :: ProductMap -> UnvalidatedOrder -> Either String [PlaceOrderEvent]
placeOrder productMap = validateOrder productMap >=> Right . createEvents
