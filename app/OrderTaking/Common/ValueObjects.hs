module OrderTaking.Common.ValueObjects (ProductCode, ProductName, createProductCode, createProductName) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype ProductCode = ProductCode Text
    deriving (Generic, Eq, Show)

newtype ProductName = ProductName {value :: Text}
    deriving (Generic, Eq, Show)

createProductCode :: Text -> Maybe ProductCode
createProductCode code =
    if not (T.null code) then Just (ProductCode code) else Nothing

createProductName :: Text -> Maybe ProductName
createProductName code =
    if not (T.null code) then Just (ProductName code) else Nothing