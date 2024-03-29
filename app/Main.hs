module Main where

import HandlePlaceOrder (handlePlaceOrder)
import OrderTaking.Workflows.PlaceOrder.Dtos.Instances ()
import Web.Scotty (
    post,
    scotty,
 )

main :: IO ()
main = scotty 3000 $ do
    post "/order" handlePlaceOrder
