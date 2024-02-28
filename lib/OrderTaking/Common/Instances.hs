{-# OPTIONS_GHC -Wno-orphans #-}

module OrderTaking.Common.Instances where

import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
 )

import OrderTaking.Common.Order (ValidatedOrder, ValidatedOrderLine)
import OrderTaking.Common.ValueObjects (ProductCode, ProductName, ProductQuantity)

-- Value objects
-- JSON encoding and decoding
instance ToJSON ProductCode where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ProductCode

instance ToJSON ProductName where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ProductName

instance ToJSON ProductQuantity where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ProductQuantity

-- Order
-- JSON encoding and decoding
instance ToJSON ValidatedOrder where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ValidatedOrder

instance ToJSON ValidatedOrderLine where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ValidatedOrderLine