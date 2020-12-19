module Data.Habulara.Internal.Writing where

import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy   as BLC
import qualified Data.Csv               as Csv
import qualified Data.Csv.Streaming     as CsvStream
import           Data.Foldable          (Foldable(toList))
import           Data.Habulara.Types    (Record)
import           System.IO              (Handle)


-- | Writes records to a file handle.
writeRecords :: MonadIO m => Handle -> Csv.Header -> CsvStream.Records Record -> m ()
writeRecords handle headers records = liftIO $ BLC.hPutStr handle $ Csv.encodeByName headers (toList records)
