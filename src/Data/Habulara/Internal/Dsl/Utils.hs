module Data.Habulara.Internal.Dsl.Utils where

import qualified Codec.Text.IConv                    as Iconv
import           Data.Bifunctor                      (Bifunctor(first))
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as BL
import qualified Data.Csv                            as Csv
import qualified Data.Csv.Streaming                  as CsvStreaming
import           Data.Habulara.Internal.Commons.Csv  (unsafeRecordsToList)
import           Data.Habulara.Internal.Dsl.Compiler (fieldSpecToMapper, fileSpecToHeader)
import           Data.Habulara.Internal.Dsl.Types    (FileSpec(..))
import           Data.Habulara.Internal.Reading      (readRecords)
import           Data.Habulara.Types                 (Record)
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import           Data.Yaml                           (decodeEither')


process
  :: FileSpec       -- ^ HAB specification
  -> BL.ByteString  -- ^ Input CSV content
  -> Either String BL.ByteString
process fs csv = case readRecordsWithGeneratedMapper fs transcoded of
  Left err -> Left err
  Right rs -> Right $ Csv.encodeByName (V.fromList (fileSpecToHeader fs)) (unsafeRecordsToList rs)
  where
    transcoded = case fileSpecEncoding fs of
      Nothing -> csv
      Just fe -> Iconv.convert (T.unpack fe) "UTF-8" csv


processWithHab
  :: B.ByteString   -- ^ HAB specification file content
  -> BL.ByteString  -- ^ Input CSV content
  -> Either String BL.ByteString
processWithHab hab csv = case readHab hab of
  Left err -> Left $ "Error while reading the HAB file: " <> err
  Right fs -> process fs csv


readHab :: B.ByteString -> Either String FileSpec
readHab = first show . decodeEither'


--------------
-- INTERNAL --
--------------

readRecordsWithGeneratedMapper :: FileSpec -> BL.ByteString -> Either String (CsvStreaming.Records Record)
readRecordsWithGeneratedMapper fs = readRecords (fmap fieldSpecToMapper (fileSpecFields fs)) (fileSpecDelimiter fs)
