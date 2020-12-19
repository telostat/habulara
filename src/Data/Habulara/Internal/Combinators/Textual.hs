{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Internal.Combinators.Textual where

import           Control.Monad.Except                       (MonadError(throwError))
import qualified Control.Monad.State                        as SM
import           Data.Habulara.Internal.Combinators.Commons (Operator, setValue)
import           Data.Habulara.Internal.Commons.Text        (nonEmptyText)
import           Data.Habulara.Types                        (DynamicContext(..), Value(..))
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as TE
import           Data.Time                                  (Day(toModifiedJulianDay))


-- ** Textual

asText :: Operator
asText = setValue =<< SM.gets (aux . dynamicContextValue)
  where
    aux :: Value -> Value
    aux VEmpty           = VEmpty
    aux (VRaw b)         = VText . TE.decodeUtf8 $ b
    aux (VInt i)         = VText . T.pack . show $ i
    aux (VText b)        = VText b
    aux (VDecimal d)     = VText . T.pack . show $ d
    aux (VBoolean False) = VText "False"
    aux (VBoolean True)  = VText "True"
    aux (VDate d)        = VText . T.pack . show $ toModifiedJulianDay d


asNEText :: Operator
asNEText = SM.gets dynamicContextValue >>= aux >>= setValue
  where
    aux VEmpty = pure VEmpty
    aux (VRaw b) = case nonEmptyText . TE.decodeUtf8 $ b of
      Nothing -> throwError "Text is empty"
      Just sv -> pure $ VText sv
    aux (VInt i) = pure $ VText . T.pack . show $ i
    aux (VText b) = case nonEmptyText b of
      Nothing -> throwError "Text is empty"
      Just sv -> pure $ VText sv
    aux (VDecimal d) = pure $ VText . T.pack . show $ d
    aux (VBoolean False) = pure $ VText "False"
    aux (VBoolean True) = pure $ VText "True"
    aux (VDate d) = pure $ VText . T.pack . show $ toModifiedJulianDay d
