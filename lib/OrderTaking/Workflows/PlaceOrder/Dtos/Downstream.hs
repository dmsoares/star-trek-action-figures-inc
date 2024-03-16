module OrderTaking.Workflows.PlaceOrder.Dtos.Downstream (PlaceOrderEvent (..), mkOrderPlacedEvent, module OP) where

import GHC.Generics (Generic)
import OrderTaking.Common.Events (Event (Event))
import OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.OrderPlacedData as OP (
    OrderLine,
    OrderPlacedData,
    mkOrderPlacedData,
 )
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder qualified as VO

newtype PlaceOrderEvent = OrderPlacedEvent (Event OrderPlacedData)
    deriving (Generic, Show)

mkOrderPlacedEvent :: VO.ValidatedOrder -> PlaceOrderEvent
mkOrderPlacedEvent = OrderPlacedEvent . Event "OrderPlaced" . mkOrderPlacedData