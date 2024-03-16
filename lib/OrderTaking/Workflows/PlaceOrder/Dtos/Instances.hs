{-# OPTIONS_GHC -Wno-orphans #-}

module OrderTaking.Workflows.PlaceOrder.Dtos.Instances where

import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
 )
import OrderTaking.Common.Instances ()
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream qualified as Downstream
import OrderTaking.Workflows.PlaceOrder.Dtos.Upstream qualified as Upstream

-- JSON encoding and decoding
instance ToJSON Upstream.Order where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Upstream.Order

instance ToJSON Upstream.OrderLine where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Upstream.OrderLine

instance ToJSON Downstream.OrderLine where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Downstream.OrderPlacedData where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Downstream.PlaceOrderEvent where
    toEncoding = genericToEncoding defaultOptions