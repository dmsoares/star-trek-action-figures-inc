module OrderTaking.Workflows.PlaceOrder.Workflow.PriceOrder where

import Data.Map qualified as M
import Data.Text (Text)
import OrderTaking.Common.Types (ProductName (unProductName), ProductQuantity (unProductQuantity), createPrice)
import OrderTaking.Workflows.PlaceOrder.Types.PricedOrder (PricedOrder (..), PricedOrderLine (..))
import OrderTaking.Workflows.PlaceOrder.Types.ValidatedOrder (ValidatedOrder (..), ValidatedOrderLine (..))

type ProductMap = M.Map Text Text

priceOrder :: ProductMap -> ValidatedOrder -> Either Text PricedOrder
priceOrder productMap ValidatedOrder{orderId, customerId, orderLines, shippingAddress} = do
    -- We're traversing the list of order lines and pricing each one.
    -- If any of them fail, we return the error. If all of them succeed, we return the priced order.
    pricedOrderLines <- traverse (priceOrderLine productMap) orderLines
    Right $
        PricedOrder
            { orderId
            , customerId
            , orderLines = pricedOrderLines
            , shippingAddress
            , -- sum the prices of all order lines
              totalPrice = sum . fmap price $ pricedOrderLines
            }

priceOrderLine :: ProductMap -> ValidatedOrderLine -> Either Text PricedOrderLine
priceOrderLine productMap ValidatedOrderLine{productName, quantity} =
    let
        quantity' = unProductQuantity quantity
        maybePrice = (* fromIntegral quantity') <$> (M.lookup (unProductName productName) productMap >>= createPrice)
        maybeLine = PricedOrderLine productName quantity <$> maybePrice
     in
        maybe (Left $ "Product not found: " <> unProductName productName) Right maybeLine
