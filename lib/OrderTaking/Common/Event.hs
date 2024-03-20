module OrderTaking.Common.Event where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)

data Event a = Event
    { eventType :: Text
    , eventData :: a
    }
    deriving (Generic, Show)

instance (ToJSON a) => ToJSON (Event a) where
    toEncoding = genericToEncoding defaultOptions
