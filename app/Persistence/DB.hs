module Persistence.DB (saveEvents, getProductCode) where

import Data.Aeson (
    ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
 )
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.IO.Strict qualified as S

data Row a = Row
    { rowId :: Int
    , rowData :: a
    }
    deriving (Generic, Show)

eventsFile :: FilePath
eventsFile = "events.txt"

saveEvents :: (ToJSON a) => [a] -> IO ()
saveEvents = traverse_ saveRow
  where
    saveRow rowData = getNewId >>= \rowId -> appendRow . encode $ Row{rowId, rowData}

getNewId :: IO Int
getNewId =
    S.readFile eventsFile >>= \contents ->
        let numberOfLines = length . lines $ contents
         in pure (numberOfLines + 1)

appendRow :: (Show a) => a -> IO ()
appendRow = B.appendFile eventsFile . fromString . (<> "\n") . show

-- Products
productsFile :: FilePath
productsFile = "products.txt"

getProductCode :: Text -> IO (Maybe Text)
getProductCode productName = do
    contents <- S.readFile productsFile
    let productLines = lines contents
    let productLines' = map words productLines
    let products = map (\(code : rest) -> (pack (concat rest), pack code)) productLines'
    let productCode = lookup productName products
    return productCode

instance (ToJSON a) => ToJSON (Row a) where
    toEncoding = genericToEncoding defaultOptions