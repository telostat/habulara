module Data.Habulara.Core.Conduit where

import Control.Monad.State (MonadIO (..), modify')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT, Void, runConduit, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Habulara.Core.Internal.Cassava (conduitEncode, sourceCassavaRecordsContents)
import Data.Habulara.Core.Mapping (FieldMapper, mapRecord)
import Data.Habulara.Core.Types.Class (HabularaError (..), HabularaT, runHabularaT)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import System.IO (Handle)


-- | Maps given CSV data into a file handle.
runMapperIntoHandle
  :: MonadIO io
  => Char
  -- ^ Delimiter
  -> Maybe String
  -- ^ Optional source text encoding
  -> [FieldMapper]
  -- ^ Field operators
  -> Bool
  -- ^ Indicates if we want the header in the output
  -> BL.ByteString
  -- ^ CSV data
  -> Handle
  -- ^ File handle to sink to
  -> io (Either HabularaError ((), Integer))
runMapperIntoHandle delim encoding ops headerp content handle = runMapperWithSink delim encoding ops headerp content (C.sinkHandle handle)


-- | Maps given CSV data and sinks via the given sink.
runMapperWithSink
  :: MonadIO io
  => Char
  -- ^ Delimiter
  -> Maybe String
  -- ^ Optional source text encoding
  -> [FieldMapper]
  -- ^ Field operators
  -> Bool
  -- ^ Indicates if we want the header in the output
  -> BL.ByteString
  -- ^ CSV data
  -> ConduitT B.ByteString Void (HabularaT () Integer io) ()
  -- ^ Sink
  -> io (Either HabularaError ((), Integer))
runMapperWithSink delim encoding ops headerP content = runHabularaConduit () 0 conduit
  where
    header = V.fromList $ fmap (TE.encodeUtf8 . fst) ops
    habularaRecordsDecode = sourceCassavaRecordsContents delim encoding content
    habularaRecordsOperate = C.mapM (\x -> modify' (1 +) >> mapRecord ops x)
    habularaRecordsEncode = conduitEncode header headerP
    conduit = habularaRecordsDecode .| habularaRecordsOperate .| habularaRecordsEncode


-- * Helpers


-- \$helpers

-- | Runs a Habulara conduit and sinks the result using a Habulara sink.
runHabularaConduit
  :: (MonadIO m)
  => r
  -- ^ Environment.
  -> s
  -- ^ Initial state.
  -> ConduitT () o (HabularaT r s m) ()
  -- ^ Conduit to run.
  -> ConduitT o Void (HabularaT r s m) a
  -- ^ Sink to use.
  -> m (Either HabularaError (a, s))
  -- ^ Either a Habulara error or a tuple of the sinked result and the final state.
runHabularaConduit env state conduit sink = runHabularaT env state (runConduit (conduit .| sink))
