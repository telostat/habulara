{-# LANGUAGE RankNTypes #-}

module Data.Habulara.Core.Mapping where

import           Control.Monad.State             (MonadState(get), modify)
import           Data.Habulara.Core.Types.Class  (OperatorM)
import           Data.Habulara.Core.Types.Record (FieldName, Record)
import           Data.Habulara.Core.Types.Value  (Value)
import qualified Data.HashMap.Strict             as HM


mapper :: OperatorM m => [(FieldName, m Value)] -> m Record
mapper []                  = get
mapper ((name, prog) : xs) = prog >>= modify . HM.insert name >> mapper xs
