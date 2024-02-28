{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >=>" #-}

module OrderTaking.Common.Order (UnvalidatedOrder (..), UnvalidatedOrderLine (..), ValidatedOrder, ValidatedOrderLine, ProductMap, validateOrder) where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import OrderTaking.Common.ValueObjects (ProductCode, ProductName, ProductQuantity, createProductCode, createProductName, createProductQuantity, unwrapProductName)

type ProductMap = [(Text, Text)]

type UnvalidatedProductName = Text
type UnvalidatedAddress = Text
type UnvalidatedQuantity = Int

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

data UnvalidatedOrder = UnvalidatedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [UnvalidatedOrderLine]
    , shippingAddress :: UnvalidatedAddress
    }
    deriving (Show)

data UnvalidatedOrderLine = UnvalidatedOrderLine
    { productName :: UnvalidatedProductName
    , quantity :: UnvalidatedQuantity
    }
    deriving (Show)

validateOrder :: ProductMap -> UnvalidatedOrder -> Either String ValidatedOrder
validateOrder productMap UnvalidatedOrder{orderId, customerId, orderLines, shippingAddress} =
    case traverse (createValidOrderLine productMap) orderLines of
        Nothing -> Left "Some order lines are invalid. Please check the product names and quantities"
        Just validatedOrderLines -> Right (ValidatedOrder{orderId, customerId, orderLines = validatedOrderLines, shippingAddress})

createValidOrderLine :: ProductMap -> UnvalidatedOrderLine -> Maybe ValidatedOrderLine
createValidOrderLine productMap (UnvalidatedOrderLine unvalidatedProductName unvalidatedQuantity) =
    validateOrderLineInfo (unvalidatedProductName, unvalidatedQuantity) >>= \(productName, quantity) ->
        lookup (unwrapProductName productName) productMap >>= \rawProductCode ->
            createProductCode rawProductCode >>= \productCode -> Just (ValidatedOrderLine{productCode, productName, quantity})

validateOrderLineInfo :: (UnvalidatedProductName, UnvalidatedQuantity) -> Maybe (ProductName, ProductQuantity)
validateOrderLineInfo (unvalidatedProductName, unvalidatedQuantity) =
    createProductName unvalidatedProductName >>= \productName ->
        createProductQuantity unvalidatedQuantity >>= \quantity ->
            Just (productName, quantity)