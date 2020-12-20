{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Habulara.Internal.Commons.Read where

import           Control.Monad.Except  (MonadError(throwError))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Habulara.Types   (HabularaErrorM)
import qualified Data.Text             as T
import           Text.Read             (readMaybe)


type ReadHM m a b = (HabularaErrorM m, Read b) => a -> m b


readHMS :: ReadHM m String a
readHMS x = case readMaybe x of
  Nothing -> throwError $ "Can not convert from value. Value was: " <> x
  Just sv -> pure sv


readHMB :: ReadHM m B.ByteString  a
readHMB = readHMS . BC.unpack


readHMT :: ReadHM m T.Text a
readHMT = readHMS . T.unpack
