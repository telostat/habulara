module Data.Habulara.Internal.Dsl.Utils where

import           Control.Monad.IO.Class              (MonadIO)
import           Data.Bifunctor                      (Bifunctor(first))
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as BL
import qualified Data.Csv.Streaming                  as CsvStreaming
import           Data.Habulara.Internal.Dsl.Compiler (fieldSpecToMapper, fileSpecToHeader)
import           Data.Habulara.Internal.Dsl.Types    (FileSpec(..))
import           Data.Habulara.Internal.Reading      (readRecords)
import           Data.Habulara.Internal.Writing      (writeRecords)
import           Data.Habulara.Types                 (Record)
import qualified Data.Vector                         as V
import           Data.Yaml                           (decodeEither')
import           System.IO                           (Handle)


readRecordsWithGeneratedMapper :: FileSpec -> BL.ByteString -> Either String (CsvStreaming.Records Record)
readRecordsWithGeneratedMapper fs = readRecords (fmap fieldSpecToMapper (fileSpecFields fs)) (fileSpecDelimiter fs)


readWriteRecordsWithGeneratedMapper :: MonadIO m => FileSpec -> BL.ByteString -> Handle -> m (Either String ())
readWriteRecordsWithGeneratedMapper fs content handle = case readRecordsWithGeneratedMapper fs content of
  Left err -> pure $ Left err
  Right rs -> Right <$> writeRecords handle (V.fromList (fileSpecToHeader fs)) rs


readHabFile :: B.ByteString -> Either String FileSpec
readHabFile = first show . decodeEither'
