module Persistence.DB (saveEvents, getProductMap) where

import Data.Aeson (
    ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
 )
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.IO.Strict qualified as S

data Row a = Row
    { rowId :: Int
    , rowData :: a
    }
    deriving (Generic, Show)

eventsFile :: FilePath
eventsFile = "events.json"

saveEvents :: (ToJSON a) => [a] -> IO ()
saveEvents = traverse_ (appendRow . encode)

appendRow :: BL.ByteString -> IO ()
appendRow = B.appendFile eventsFile . (<> "\n") . BL.toStrict

-- Products
productsFile :: FilePath
productsFile = "products.txt"

getProductMap :: IO [(Text, Text)]
getProductMap = do
    contents <- S.readFile productsFile
    let productLines = lines contents
    let productLines' = map words productLines
    let productMap = map (\(code : rest) -> (pack (concat rest), pack code)) productLines'
    return productMap

instance (ToJSON a) => ToJSON (Row a) where
    toEncoding = genericToEncoding defaultOptions