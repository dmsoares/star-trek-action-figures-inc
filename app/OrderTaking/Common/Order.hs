{-# LANGUAGE LambdaCase #-}

module OrderTaking.Common.Order (GetProductCode, UnvalidatedOrder (..), UnvalidatedOrderLine (..), ValidatedOrder, ValidatedOrderLine, validateOrder) where

import Data.Text (Text, unpack)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type ProductCode = Text
type ProductName = Text

data ValidatedOrder = ValidatedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [ValidatedOrderLine]
    , shippingAddress :: Text
    }
    deriving (Generic, Show)

data ValidatedOrderLine = ValidatedOrderLine
    { productCode :: Text
    , productName :: Text
    , quantity :: Int
    }
    deriving (Generic, Show)

data UnvalidatedOrder = UnvalidatedOrder
    { orderId :: UUID
    , customerId :: Int
    , orderLines :: [UnvalidatedOrderLine]
    , shippingAddress :: Text
    }
    deriving (Show)

data UnvalidatedOrderLine = UnvalidatedOrderLine
    { productName :: Text
    , quantity :: Int
    }
    deriving (Show)

type GetProductCode = ProductName -> IO (Maybe ProductCode)

validateOrder :: GetProductCode -> UnvalidatedOrder -> IO (Either String ValidatedOrder)
validateOrder getProductCode UnvalidatedOrder{orderId, customerId, orderLines, shippingAddress} =
    traverse (validateOrderLine getProductCode) orderLines
        >>= ( \case
                Left err -> return $ Left err
                Right validatedOrderLines -> return $ Right (ValidatedOrder{orderId, customerId, orderLines = validatedOrderLines, shippingAddress})
            )
        . sequence

validateOrderLine :: GetProductCode -> UnvalidatedOrderLine -> IO (Either String ValidatedOrderLine)
validateOrderLine getProductCode (UnvalidatedOrderLine{productName, quantity}) =
    getProductCode productName >>= \case
        Nothing -> return $ Left ("Invalid product name: " <> unpack productName)
        Just productCode -> return $ Right (ValidatedOrderLine{productCode, productName, quantity})