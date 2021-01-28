{-# LANGUAGE FlexibleContexts #-}

module Data.Habulara.Core.Internal.Cassava where

import           Control.Monad            (when, (>=>))
import           Control.Monad.Except     (MonadError(throwError))
import           Control.Monad.IO.Class   (MonadIO(liftIO))
import qualified Data.ByteString          as B
import qualified Data.ByteString.Builder  as B.Builder
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (ord)
import           Data.Conduit             (ConduitT, yield)
import qualified Data.Conduit.Combinators as C
import qualified Data.Csv                 as Cassava
import qualified Data.Csv.Builder         as Cassava.Builder
import qualified Data.Csv.Streaming       as Cassava.Streaming
import           Data.Habulara.Core.Class (HabularaError(HabularaErrorCsv))
import           System.IO                (Handle, IOMode(ReadMode), hClose, openFile)


sourceCassavaRecordsFilePath
  :: (MonadIO m, MonadError HabularaError m, Cassava.FromNamedRecord a)
  => Char
  -> FilePath
  -> ConduitT i a m ()
sourceCassavaRecordsFilePath delim filepath = do
  handle <- liftIO $ openFile filepath ReadMode
  sourceCassavaRecordsHandle delim handle <* liftIO (hClose handle)


sourceCassavaRecordsHandle
  :: (MonadIO m, MonadError HabularaError m, Cassava.FromNamedRecord a)
  => Char
  -> Handle
  -> ConduitT i a m ()
sourceCassavaRecordsHandle delim = liftIO . BL.hGetContents >=> sourceCassavaRecordsContents delim


sourceCassavaRecordsContents
  :: (MonadError HabularaError m, Cassava.FromNamedRecord a)
  => Char
  -> BL.ByteString
  -> ConduitT i a m ()
sourceCassavaRecordsContents delim content =
  either (throwError . HabularaErrorCsv) (sourceCassavaStream . snd) dresult
  where
    options = Cassava.defaultDecodeOptions { Cassava.decDelimiter = fromIntegral (ord delim) }
    dresult = Cassava.Streaming.decodeByNameWith options content


sourceCassavaStream
  :: (MonadError HabularaError m, Cassava.FromNamedRecord a)
  => Cassava.Streaming.Records a
  -> ConduitT i a m ()
sourceCassavaStream (Cassava.Streaming.Nil Nothing _)     = return ()
sourceCassavaStream (Cassava.Streaming.Nil (Just err) _)  = throwError $ HabularaErrorCsv err
sourceCassavaStream (Cassava.Streaming.Cons (Left err) _) = throwError $ HabularaErrorCsv err
sourceCassavaStream (Cassava.Streaming.Cons (Right r) rs) = yield r >> sourceCassavaStream rs


conduitEncode
  :: (Monad m, Cassava.ToNamedRecord i)
  => Cassava.Header
  -> Bool
  -> ConduitT i B.ByteString m ()
conduitEncode header headerP = when headerP (conduitEncodeHeader header) >> conduitEncodeRecords header


conduitEncodeHeader :: Monad m => Cassava.Header -> ConduitT i B.ByteString m ()
conduitEncodeHeader = yield . BL.toStrict . B.Builder.toLazyByteString . Cassava.Builder.encodeHeader


conduitEncodeRecords :: (Monad m, Cassava.ToNamedRecord i) => Cassava.Header -> ConduitT i B.ByteString m ()
conduitEncodeRecords = C.map . encodeRecord


-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.HashMap.Strict as HM
-- >>> let header1 = V.fromList ["a", "b"] :: Cassava.Header
-- >>> let header2 = V.fromList ["b", "a"] :: Cassava.Header
-- >>> let record = HM.fromList [("a", "A"), ("b", "B")] :: HM.HashMap B.ByteString B.ByteString
-- >>> encodeRecord header1 record
-- "A,B\r\n"
-- >>> encodeRecord header2 record
-- "B,A\r\n"
encodeRecord :: Cassava.ToNamedRecord a => Cassava.Header -> a -> B.ByteString
encodeRecord header = BL.toStrict . B.Builder.toLazyByteString . Cassava.Builder.encodeNamedRecord header
