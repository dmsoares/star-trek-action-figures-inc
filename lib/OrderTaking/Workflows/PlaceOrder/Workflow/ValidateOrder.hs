{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >=>" #-}

module OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder (
    ValidatedOrder,
    ValidatedOrderLine,
    ProductMap,
    orderId,
    customerId,
    orderLines,
    shippingAddress,
    productCode,
    productName,
    quantity,
    validateOrder,
) where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import OrderTaking.Common.Types (
    ProductCode,
    ProductName,
    ProductQuantity,
    createProductCode,
    createProductName,
    createProductQuantity,
    unwrapProductName,
 )
import OrderTaking.Workflows.PlaceOrder.Types.UnvalidatedOrder qualified as UO

-- | A validated order is an order that has been checked for correctness
data ValidatedOrder = ValidatedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [ValidatedOrderLine]
    , shippingAddress :: Text
    }
    deriving (Generic, Show)

data ValidatedOrderLine = ValidatedOrderLine
    { productCode :: ProductCode
    , productName :: ProductName
    , quantity :: ProductQuantity
    }
    deriving (Generic, Show)

-- | A product map is a list of tuples where the first element is the product name and the second element is the product code
type ProductMap = [(Text, Text)]

-- | Validate an order
validateOrder :: ProductMap -> UO.UnvalidatedOrder -> Either String ValidatedOrder
validateOrder productMap UO.UnvalidatedOrder{orderId, customerId, orderLines, shippingAddress} =
    case traverse (createValidOrderLine productMap) orderLines of
        Nothing -> Left "Some order lines are invalid. Please check the product names and quantities"
        Just validatedOrderLines -> Right (ValidatedOrder{orderId, customerId, orderLines = validatedOrderLines, shippingAddress})

createValidOrderLine :: ProductMap -> UO.UnvalidatedOrderLine -> Maybe ValidatedOrderLine
createValidOrderLine productMap (UO.UnvalidatedOrderLine unvalidatedProductName unvalidatedQuantity) =
    validateOrderLineInfo (unvalidatedProductName, unvalidatedQuantity)
        >>= \(productName, quantity) ->
            lookup (unwrapProductName productName) productMap
                >>= \rawProductCode ->
                    createProductCode rawProductCode
                        >>= \productCode -> Just (ValidatedOrderLine{productCode, productName, quantity})

validateOrderLineInfo :: (UO.UnvalidatedProductName, UO.UnvalidatedQuantity) -> Maybe (ProductName, ProductQuantity)
validateOrderLineInfo (unvalidatedProductName, unvalidatedQuantity) =
    createProductName unvalidatedProductName
        >>= \productName ->
            createProductQuantity unvalidatedQuantity
                >>= \quantity ->
                    Just (productName, quantity)