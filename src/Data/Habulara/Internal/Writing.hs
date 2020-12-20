module Data.Habulara.Internal.Writing where

import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy   as BLC
import qualified Data.Csv               as Csv
import qualified Data.Csv.Streaming     as CsvStream
import           Data.Habulara.Types    (Record)
import           System.IO              (Handle)


-- | Writes records to a file handle.
writeRecords :: MonadIO m => Handle -> Csv.Header -> CsvStream.Records Record -> m ()
writeRecords handle headers records = liftIO $ BLC.hPutStr handle $ Csv.encodeByName headers (unsafeRecordsToList records)


-- | Lazily converts a 'Records' of @a@ to a list of @a@.
--
-- This is an unsafe function that throws an error if it encounters a parsing
-- error.
unsafeRecordsToList :: CsvStream.Records a -> [a]
unsafeRecordsToList (CsvStream.Nil Nothing _)     = []
unsafeRecordsToList (CsvStream.Nil (Just err) _)  = error err
unsafeRecordsToList (CsvStream.Cons (Left err) _) = error err
unsafeRecordsToList (CsvStream.Cons (Right r) rs) = r : unsafeRecordsToList rs
