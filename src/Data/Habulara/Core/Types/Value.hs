{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Core.Types.Value where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv              as Csv
import           Data.Scientific       (Scientific)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time             (Day, LocalTime)


-- | Habulara row record field value type.
data Value =
    VEmpty
  | VInt      !Integer
  | VText     !T.Text
  | VDecimal  !Scientific
  | VBoolean  !Bool
  | VDate     !Day
  | VDateTime !LocalTime
  deriving (Show)


-- | @cassava@ 'Csv.FromField' instance for 'Value' values.
--
-- We are parsing the CSV column into either of:
--
-- 1. 'VEmpty' if the column value is an empty text, or
-- 2. 'VText' otherwise.
--
-- >>> import Data.Csv (parseField, runParser)
-- >>> runParser $ parseField "" :: Either String Value
-- Right VEmpty
-- >>> runParser $ parseField " " :: Either String Value
-- Right (VText " ")
-- >>> runParser $ parseField " a " :: Either String Value
-- Right (VText " a ")
-- >>> runParser $ parseField " \t\r\n " :: Either String Value
-- Right (VText " \t\r\n ")
instance Csv.FromField Value where
  -- TODO: Use decodeUtf8' to catch UnicodeException and propagate.
  parseField x
    | B.null x  = pure VEmpty
    | otherwise = pure . VText . TE.decodeUtf8 $ x


-- | @cassava@ 'Csv.ToField' instance for 'Value' values.
--
-- >>> import Data.Csv (toField)
-- >>> toField VEmpty
-- ""
-- >>> toField $ VInt 42
-- "42"
-- >>> toField $ VText "Hello"
-- "Hello"
-- >>> toField $ VDecimal (read "42")
-- "42.0"
-- >>> toField $ VBoolean True
-- "True"
-- >>> toField $ VBoolean False
-- "False"
-- >>> toField $ VDate (read "2021-01-01")
-- "2021-01-01"
-- >>> toField $ VDateTime (read "2021-01-01 23:59:59")
-- "2021-01-01 23:59:59"
instance Csv.ToField Value where
  toField VEmpty           = B.empty
  toField (VInt i)         = BC.pack $ show i
  toField (VText t)        = TE.encodeUtf8 t
  toField (VDecimal d)     = BC.pack $ show d
  toField (VBoolean True)  = "True"
  toField (VBoolean False) = "False"
  toField (VDate d)        = BC.pack $ show d
  toField (VDateTime x)    = BC.pack $ show x
