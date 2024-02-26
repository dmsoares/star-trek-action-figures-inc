module OrderTaking.PlaceOrder.Workflow where

import OrderTaking.Common.Order (GetProductCode, UnvalidatedOrder, ValidatedOrder, validateOrder)
import OrderTaking.PlaceOrder.PublicTypes (DomainEvent (DomainEvent))

type OrderPlacedEvent = DomainEvent ValidatedOrder

placeOrder :: GetProductCode -> UnvalidatedOrder -> IO (Either String ValidatedOrder)
placeOrder = validateOrder

createEvents :: ValidatedOrder -> [DomainEvent ValidatedOrder]
createEvents validatedOrder = [DomainEvent "OrderPlaced" validatedOrder]