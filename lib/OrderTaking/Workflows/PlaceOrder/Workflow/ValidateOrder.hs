module OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder where

import OrderTaking.Common.Types (
    createAddress,
    createProductName,
    createProductQuantity,
 )
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder qualified as UO
import OrderTaking.Workflows.PlaceOrder.Types.ValidatedOrder (ValidatedOrder (..), ValidatedOrderLine (..))

-- | Validate an order
validateOrder :: UO.UnvalidatedOrder -> Either String ValidatedOrder
validateOrder UO.UnvalidatedOrder{orderId, customerId, orderLines, shippingAddress} =
    let maybeValidatedOrder =
            -- \ Try to create a order from order lines and shipping address
            ValidatedOrder orderId customerId
                <$> traverse createValidOrderLine orderLines
                <*> createAddress shippingAddress
     in -- This is a common pattern in Haskell to convert a Maybe to an Either
        -- If the whole computation fails with a Nothing (of type Maybe), return a Left (of type Either) with an error message
        -- otherwise, return a Right with the result
        maybe invalidOrderError Right maybeValidatedOrder
  where
    invalidOrderError = Left "Some order lines are invalid. Please check the product names and quantities"

    -- \ Try to create a validated order line from an unvalidated order line
    -- Return (Just validatedOrder) if successful, Nothing if not
    createValidOrderLine :: UO.UnvalidatedOrderLine -> Maybe ValidatedOrderLine
    createValidOrderLine
        (UO.UnvalidatedOrderLine unvalidatedProductName unvalidatedQuantity) =
            -- what we are expressing here is:
            -- try to build a ValidatedOrderLine from the productCode, productName and quantity
            -- taking into account that any of these steps could fail
            -- and, if any of them fail, return a Nothing
            ValidatedOrderLine
                <$> createProductName unvalidatedProductName -- create the product name
                <*> createProductQuantity unvalidatedQuantity -- create the product quantity
