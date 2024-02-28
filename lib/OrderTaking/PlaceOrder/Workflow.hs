module OrderTaking.PlaceOrder.Workflow where

import Control.Monad ((>=>))
import OrderTaking.Common.Order (ProductMap, UnvalidatedOrder, ValidatedOrder, validateOrder)
import OrderTaking.PlaceOrder.PublicTypes (DomainEvent, createOrderPlacedEvent)

placeOrder :: ProductMap -> UnvalidatedOrder -> Either String [DomainEvent]
placeOrder productMap = validateOrder productMap >=> Right . createEvents

createEvents :: ValidatedOrder -> [DomainEvent]
createEvents validatedOrder =
    let orderPlaced = createOrderPlacedEvent validatedOrder
     in [orderPlaced]