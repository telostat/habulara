{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Char                  (ord)
import           Data.Csv                   (DecodeOptions(decDelimiter), Header, defaultDecodeOptions, encodeByName)
import           Data.Csv.Streaming         (Records(..), decodeByNameWith)
import           Data.Foldable              (Foldable(toList))
import qualified Data.HashMap.Strict        as HM
import           Data.Scientific            (Scientific)
import           System.IO                  (Handle)
import           Text.Read                  (readMaybe)


-- | Type of a raw record.
--
-- A 'Record' is a 'HM.HashMap' which is identical to the underlying data type of @cassava@ named records.
type Record = HM.HashMap RecordField RecordValue


-- | Type of the record field.
type RecordField = B.ByteString


-- | Type of the record value.
type RecordValue = B.ByteString


-- | Type of a single field parser.
type FieldParser a = Record -> (RecordField, a) -> Either String (RecordField, a)


-- | Type of a record parser that consists of multiple 'FieldParser' values.
type RecordParser = [FieldParser RecordValue]


-- | Composes two field parsers.
compose :: FieldParser RecordValue -> FieldParser RecordValue -> FieldParser RecordValue
compose p1 p2 r p = case p1 r p of
  Left err -> Left err
  Right b -> case p2 r b of
    Left err -> Left err
    Right x  -> Right x


-- | A 'FieldParser' combinator that selects a field value from a raw record.
select :: RecordField -> FieldParser RecordValue
select f r _ = case HM.lookup f r of
  Nothing -> Left $ "Unknown field: " <> BC.unpack f
  Just rv -> Right (f, rv)


-- | A 'FieldParser' combinator that parses a decimal value from a given field value.
decimal :: FieldParser RecordValue
decimal _ (f, k) = case readMaybe (BC.unpack k) of
  Nothing -> Left $ "Can not read decimal: " <> show k
  Just dv -> Right (f, BC.pack (show (dv :: Scientific)))


-- | A 'FieldParser' combinator that does addition.
add :: Scientific -> FieldParser RecordValue
add x _ (f, k) = case readMaybe (BC.unpack k) of
  Nothing -> Left $ "Can not add value (not a decimal): " <> show k
  Just dv -> Right (f, BC.pack (show (dv + x)))


-- | A 'FieldParser' combinator that does subtraction.
subtract :: Scientific -> FieldParser RecordValue
subtract x _ (f, k) = case readMaybe (BC.unpack k) of
  Nothing -> Left $ "Can not subtract value (not a decimal): " <> show k
  Just dv -> Right (f, BC.pack (show (dv - x)))


-- | A 'FieldParser' combinator that does multiplication.
multiply :: Scientific -> FieldParser RecordValue
multiply x _ (f, k) = case readMaybe (BC.unpack k) of
  Nothing -> Left $ "Can not multiply value (not a decimal): " <> show k
  Just dv -> Right (f, BC.pack (show (dv * x)))


-- | A 'FieldParser' combinator that does division.
--
-- TODO: provide a safe division operation.
divide :: Scientific -> FieldParser RecordValue
divide x _ (f, k) = case readMaybe (BC.unpack k) of
  Nothing -> Left $ "Can not multiply value (not a decimal): " <> show k
  Just dv -> Right (f, BC.pack (show (dv / x)))


-- | A 'FieldParser' combinator that converts a decimal value to percentage points, ie. multiplies by @100@.
percentagePoints :: FieldParser RecordValue
percentagePoints = multiply 100


-- | A 'FieldParser' combinator that renames the field.
rename :: B.ByteString -> FieldParser RecordValue
rename n _ (_, k) = Right (n, k)


-- | Parses a record.
parse :: RecordParser -> Record -> Either String Record
parse = parse' HM.empty


-- | Auxiliary function for the main 'parse' function.
parse' :: Record -> RecordParser -> Record -> Either String Record
parse' acc [] _        = Right acc
parse' acc (p:ps) raw = case epv of
  Left err     -> Left err
  Right (f, v) -> parse' (HM.insert f v acc) ps raw
  where
    epv = p raw ("", "")


-- | Reads raw records.
readRawRecords :: Char -> BL.ByteString -> Either String (Header, Records Record)
readRawRecords = decodeByNameWith . mkOptions


-- | Reads and parses records.
readRecords :: RecordParser -> Char -> BL.ByteString -> Either String (Records Record)
readRecords parser delim content = case rawRecords of
  Left err      -> Left err
  Right (_, rs) -> Right $ parseRecords parser rs
  where
    rawRecords = readRawRecords delim content


-- | Auxiliary function to parse records.
parseRecords :: RecordParser -> Records Record -> Records Record
parseRecords _ rs@(Nil _ _) = rs
parseRecords _ (Cons (Left err) _) = Cons (Left err) (Nil Nothing BL.empty)
parseRecords parser (Cons (Right r) rs) = case parse parser r of
  Left err -> Cons (Left err) (Nil Nothing BL.empty)
  Right pr -> Cons (Right pr) $ parseRecords parser rs


-- | Writes records to a file handle.
writeRecords :: MonadIO m => Handle -> Header -> Records Record -> m ()
writeRecords handle headers records = liftIO $ BLC.hPutStr handle $ encodeByName headers (toList records)


--------------
-- INTERNAL --
--------------


-- | Builds 'DecodeOptions' for the given delimiter.
mkOptions :: Char -> DecodeOptions
mkOptions sep = defaultDecodeOptions { decDelimiter = fromIntegral (ord sep) }
