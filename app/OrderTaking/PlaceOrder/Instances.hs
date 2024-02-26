{-# OPTIONS_GHC -Wno-orphans #-}

module OrderTaking.PlaceOrder.Instances where

import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
 )
import OrderTaking.PlaceOrder.Dto (OrderDto, OrderLineDto)

-- JSON encoding and decoding
instance ToJSON OrderDto where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON OrderDto

instance ToJSON OrderLineDto where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON OrderLineDto