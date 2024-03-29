module OrderTaking.Common.Event where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A generic event DTO type
data EventDto a = EventDto
    { eventType :: Text
    , eventData :: a
    }
    deriving (Generic, Show)

instance (ToJSON a) => ToJSON (EventDto a) where
    toEncoding = genericToEncoding defaultOptions
