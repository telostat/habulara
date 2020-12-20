{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Habulara.Internal.Operators.Date where

import           Control.Monad.Except                   (MonadError(throwError))
import qualified Data.ByteString.Char8                  as BC
import           Data.Habulara.Internal.Commons.Convert (parseDateFromString)
import           Data.Habulara.Types                    (Value(..), ValueOperator)
import qualified Data.Text                              as T


-- >>> trim $ raw " a " :: Either String Value
-- Right (VRaw "a")
parseDate :: String -> ValueOperator
parseDate _ VEmpty      = pure VEmpty
parseDate fmt (VText x) = VDate <$> parseDateFromString fmt (T.unpack x)
parseDate fmt (VRaw x)  = VDate <$> parseDateFromString fmt (BC.unpack x)
parseDate _ _           = throwError "Operator can only be applied to values of textual types."
