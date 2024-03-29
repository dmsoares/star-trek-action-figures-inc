module OrderTaking.Common.Types (
    ProductCode,
    ProductName,
    ProductQuantity,
    Address,
    Price,
    createProductCode,
    createProductName,
    createProductQuantity,
    createAddress,
    createPrice,
    unProductQuantity,
    unProductName,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- These types are not specific to any workflow.

-- More importantly, they are an example of a common pattern in Haskell to make illegal states unrepresentable.
-- The trick is not to expose the constructors of the types, but to provide functions to create them.
-- These are known as smart constructors and can do runtime checks to ensure that the values are valid under our domain.

-- For example, we do not accept empty product names, so we have a runtime check for that.
-- So, anytime we have a ProductName outside this module, we can be sure that it is not empty.

newtype ProductName = ProductName {unProductName :: Text}
    deriving (Eq)

newtype ProductCode = ProductCode Text
    deriving (Eq)

newtype ProductQuantity = ProductQuantity {unProductQuantity :: Int}
    deriving (Num, Eq)

newtype Address = Address Text
    deriving (Eq)

newtype Price = Price Double
    deriving (Num, Eq)

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

createPrice :: Text -> Maybe Price
createPrice price = Price <$> readMaybe (T.unpack price)

-- Instances
instance Show ProductName where
    show (ProductName name) = show name

instance Show ProductCode where
    show (ProductCode code) = show code

instance Show ProductQuantity where
    show (ProductQuantity quantity) = show quantity

instance Show Address where
    show (Address address) = show address

instance Show Price where
    show (Price price) = show price
