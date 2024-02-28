module OrderTaking.Common.ValueObjectsSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import OrderTaking.Common.ValueObjects (createAddress, createProductQuantity)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Testable (property))

spec :: Spec
spec = describe "ValueObjects" $ do
    it "createProductQuantity" $ do
        property $ \quantity ->
            let result = createProductQuantity quantity
             in if quantity > 0 && quantity <= 100
                    then isJust result
                    else isNothing result

    it "createAddress" $ do
        property $ \rawAddress ->
            let result = createAddress rawAddress
             in if not (T.null rawAddress) && T.length rawAddress <= 100
                    then isJust result
                    else isNothing result