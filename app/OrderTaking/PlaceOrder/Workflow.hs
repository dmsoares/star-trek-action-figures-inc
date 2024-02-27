module OrderTaking.PlaceOrder.Workflow where

import Control.Monad ((>=>))
import OrderTaking.Common.Order (ProductMap, UnvalidatedOrder, ValidatedOrder, validateOrder)
import OrderTaking.PlaceOrder.PublicTypes (DomainEvent (DomainEvent))

type OrderPlacedEvent = DomainEvent ValidatedOrder

placeOrder :: ProductMap -> UnvalidatedOrder -> Either String [DomainEvent ValidatedOrder]
placeOrder productMap = validateOrder productMap >=> Right . createEvents

createEvents :: ValidatedOrder -> [DomainEvent ValidatedOrder]
createEvents validatedOrder = [DomainEvent "OrderPlaced" validatedOrder]