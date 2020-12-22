module Data.Habulara.Internal.Commons.Csv where

import qualified Data.Csv.Streaming as CsvStream


-- | Lazily converts a 'Records' of @a@ to a list of @a@.
--
-- This is an unsafe function that throws an error if it encounters a parsing
-- error.
--
-- TODO: Think of a safe version (Are we going to use a streaming library with Error effects?)
unsafeRecordsToList :: CsvStream.Records a -> [a]
unsafeRecordsToList (CsvStream.Nil Nothing _)     = []
unsafeRecordsToList (CsvStream.Nil (Just err) _)  = error err
unsafeRecordsToList (CsvStream.Cons (Left err) _) = error err
unsafeRecordsToList (CsvStream.Cons (Right r) rs) = r : unsafeRecordsToList rs
