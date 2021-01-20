module Data.Habulara.Core.Types.Record where

import           Data.Habulara.Core.Types.Value (Value)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T


-- | Habulara row record type.
type Record = HM.HashMap FieldName Value


-- | Habulara row record field name type.
type FieldName = T.Text
