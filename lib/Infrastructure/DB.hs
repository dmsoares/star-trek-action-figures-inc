module Infrastructure.DB (saveEvents, getProductMap) where

import Data.Aeson (
    ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
 )
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Text (Text, pack, split)
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

type ProductMap = M.Map Text Text

getProductMap :: IO (Maybe ProductMap)
getProductMap = do
    contents <- S.readFile productsFile
    let productLines = traverse (toTuple . split (== ',') . pack) (lines contents)
    pure $ M.fromList <$> productLines
  where
    toTuple [a, b] = Just (a, b)
    toTuple _ = Nothing

instance (ToJSON a) => ToJSON (Row a) where
    toEncoding = genericToEncoding defaultOptions