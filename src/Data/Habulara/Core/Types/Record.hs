module Data.Habulara.Core.Types.Record where

import Data.Habulara.Core.Types.Value (Value)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


-- | Habulara row record type.
--
-- A row record is a 'HM.HashMap' of keys of type 'Label' and values of type
-- 'Value'.
type Record = HM.HashMap Label Value


-- | Habulara row record field name type (key type).
type Label = T.Text
