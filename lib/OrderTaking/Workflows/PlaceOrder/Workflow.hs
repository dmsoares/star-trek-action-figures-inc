module OrderTaking.Workflows.PlaceOrder.Workflow where

import Control.Monad ((>=>))
import Data.Text (Text)
import OrderTaking.Workflows.PlaceOrder.Types.Events (PlaceOrderEvent)
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder (UnvalidatedOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.CreateEvents (createEvents)
import OrderTaking.Workflows.PlaceOrder.Workflow.PriceOrder (ProductMap, priceOrder)
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder (validateOrder)

-- The workflow for placing an order.
-- It takes an unvalidated order and returns a list of events that other hypothetical services could consume.

-- The function is a composition of the following functions:
-- 1. validateOrder :: UnvalidatedOrder -> Either Text ValidatedOrder
-- 2. priceOrder productMap :: ValidatedOrder -> Either Text PricedOrder
-- 3. createEvents :: PricedOrder -> [PlaceOrderEvent]

-- If we abstract away the Either 'effect', we can see a transformation pipeline,
-- sequentially transforming data from one type to another, until we have the final result:

-- UnvalidatedOrder ---> ValidatedOrder ---> PricedOrder ---> [PlaceOrderEvent]

-- Now, we know that both 'validateOrder' and 'priceOrder' can fail, so we wrap them in 'Either Text'.
-- the (>=>) operator allows us to chain functions that return 'Either Text' values (monadic values, more generally).
-- If any of the functions in the chain returns a 'Left', the chain is broken and the 'Left' value is returned.
-- If all functions return 'Right x', the 'x' value is passed along to the next computation until the end of the chain.

placeOrder :: ProductMap -> UnvalidatedOrder -> Either Text [PlaceOrderEvent]
placeOrder productMap = validateOrder >=> priceOrder productMap >=> Right . createEvents
