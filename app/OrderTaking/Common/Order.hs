module OrderTaking.Common.Order (UnvalidatedOrder (..), UnvalidatedOrderLine (..), ValidatedOrder, ValidatedOrderLine, ProductMap, validateOrder) where

import Data.Text (Text, unpack)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type ProductCode = Text
type ProductName = Text

type ProductMap = [(ProductName, ProductCode)]

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

validateOrder :: ProductMap -> UnvalidatedOrder -> Either String ValidatedOrder
validateOrder productMap UnvalidatedOrder{orderId, customerId, orderLines, shippingAddress} =
    case traverse (validateOrderLine productMap) orderLines of
        Left err -> Left err
        Right validatedOrderLines -> Right (ValidatedOrder{orderId, customerId, orderLines = validatedOrderLines, shippingAddress})

validateOrderLine :: ProductMap -> UnvalidatedOrderLine -> Either String ValidatedOrderLine
validateOrderLine productMap (UnvalidatedOrderLine{productName, quantity}) =
    case lookup productName productMap of
        Nothing -> Left ("Invalid product name: " <> unpack productName)
        Just productCode -> Right (ValidatedOrderLine{productCode, productName, quantity})