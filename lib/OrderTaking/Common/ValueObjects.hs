module OrderTaking.Common.ValueObjects (
    ProductCode,
    ProductName,
    ProductQuantity,
    Address,
    createProductCode,
    createProductName,
    createProductQuantity,
    createAddress,
    unwrapProductName,
) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype ProductName = ProductName Text
    deriving (Generic, Eq, Show)

newtype ProductCode = ProductCode Text
    deriving (Generic, Eq, Show)

newtype ProductQuantity = ProductQuantity Int
    deriving (Generic, Eq, Show)

newtype Address = Address Text
    deriving (Generic, Eq, Show)

createProductName :: Text -> Maybe ProductName
createProductName name =
    if not (T.null name)
        then Just (ProductName name)
        else Nothing

createProductCode :: Text -> Maybe ProductCode
createProductCode code
    | T.null code || T.take 1 code /= "C" = Nothing
    | otherwise = Just (ProductCode code)

createProductQuantity :: Int -> Maybe ProductQuantity
createProductQuantity quantity =
    if quantity > 0 && quantity <= 100
        then Just (ProductQuantity quantity)
        else Nothing

createAddress :: Text -> Maybe Address
createAddress address =
    let addressLength = T.length address
     in if addressLength > 0 && addressLength <= 150
            then Just (Address address)
            else Nothing

unwrapProductName :: ProductName -> Text
unwrapProductName (ProductName name) = name