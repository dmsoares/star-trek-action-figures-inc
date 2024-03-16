{-# OPTIONS_GHC -Wno-orphans #-}

module OrderTaking.Common.Instances where

import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
 )
import Data.Text ()
import OrderTaking.Common.Types (
    ProductCode,
    ProductName,
    ProductQuantity,
 )

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
