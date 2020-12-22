module Data.Habulara.Internal.Reading where

import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (ord)
import qualified Data.Csv                       as Csv
import qualified Data.Csv.Streaming             as CsvStream
import           Data.Habulara.Internal.Mapping (mapRecord)
import           Data.Habulara.Types            (FieldMappers, Record)


-- | Reads and maps records.
readRecords :: FieldMappers -> Char -> BL.ByteString -> Either String (CsvStream.Records Record)
readRecords parser delim content = case rawRecords of
  Left err      -> Left err
  Right (_, rs) -> Right $ mapRecords parser rs
  where
    rawRecords = readRawRecords delim content


-- | Reads raw records.
readRawRecords :: Char -> BL.ByteString -> Either String (Csv.Header, CsvStream.Records Record)
readRawRecords = CsvStream.decodeByNameWith . mkOptions
  where
    mkOptions sep = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord sep) }


-- | Auxiliary function to map records.
mapRecords :: FieldMappers -> CsvStream.Records Record -> CsvStream.Records Record
mapRecords _ (CsvStream.Nil x y)                = CsvStream.Nil x y
mapRecords _ (CsvStream.Cons (Left err) _)      = CsvStream.Cons (Left err) (CsvStream.Nil Nothing BL.empty)
mapRecords parser (CsvStream.Cons (Right r) rs) = case mapRecord r parser of
  Left err -> CsvStream.Cons (Left err) (CsvStream.Nil Nothing BL.empty)
  Right pr -> CsvStream.Cons (Right pr) $ mapRecords parser rs
