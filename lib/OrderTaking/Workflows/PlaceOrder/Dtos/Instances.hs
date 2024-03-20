{-# OPTIONS_GHC -Wno-orphans #-}

module OrderTaking.Workflows.PlaceOrder.Dtos.Instances where

import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
 )
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream (PlaceOrderEventDto)
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.OrderPlacedDto qualified as OP
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.ShippableOrderPlacedDto qualified as SOP
import OrderTaking.Workflows.PlaceOrder.Dtos.Upstream qualified as Upstream

-- JSON encoding and decoding
instance FromJSON Upstream.OrderDto

instance FromJSON Upstream.OrderLineDto

instance ToJSON OP.OrderLineDto where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON OP.OrderPlacedDto where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON SOP.OrderLineDto where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON SOP.ShippableOrderPlacedDto where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON PlaceOrderEventDto where
    toEncoding = genericToEncoding defaultOptions