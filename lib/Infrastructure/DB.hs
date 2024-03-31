module Infrastructure.DB (saveEvents, getProductMap) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, split)
import System.IO.Strict qualified as S

eventsFile :: FilePath
eventsFile = "db/events.json"

saveEvents :: (Show a, ToJSON a) => [a] -> IO ()
saveEvents events = do
    let encodedEvents = fmap encode events
    traverse_ appendAndPrint encodedEvents
  where
    appendAndPrint event = appendRow event >> print event

appendRow :: BL.ByteString -> IO ()
appendRow = B.appendFile eventsFile . (<> "\n") . BL.toStrict

-- Products
productsFile :: FilePath
productsFile = "db/products.txt"

type ProductMap = M.Map Text Text

getProductMap :: IO ProductMap
getProductMap = M.fromList . toProductLines <$> S.readFile productsFile
  where
    toProductLines = mapMaybe (toTuple . split (== ',') . pack) . lines
    toTuple [a, b] = Just (a, b)
    toTuple _ = Nothing
