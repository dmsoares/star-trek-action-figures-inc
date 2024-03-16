module OrderTaking.Workflows.PlaceOrder.Dtos.Downstream.OrderPlacedData where

import GHC.Generics (Generic)
import OrderTaking.Workflows.PlaceOrder.Workflow.ValidateOrder qualified as VO

data OrderPlacedData = OrderPlacedData
    { orderId :: String
    , customerId :: String
    , shippingAddress :: String
    , orderLines :: [OrderLine]
    }
    deriving (Generic, Show)

data OrderLine = OrderLine
    { productCode :: String
    , productName :: String
    , quantity :: String
    }
    deriving (Generic, Show)

mkOrderPlacedData :: VO.ValidatedOrder -> OrderPlacedData
mkOrderPlacedData order =
    OrderPlacedData
        (show $ VO.orderId order)
        (show $ VO.customerId order)
        (show $ VO.shippingAddress order)
        (map createOrderPlacedLine $ VO.orderLines order)
  where
    createOrderPlacedLine :: VO.ValidatedOrderLine -> OrderLine
    createOrderPlacedLine line =
        OrderLine
            (show $ VO.productCode line)
            (show $ VO.productName line)
            (show $ VO.quantity line)