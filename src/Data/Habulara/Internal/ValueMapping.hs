{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Internal.ValueMapping where

import           Control.Monad.Except                   (MonadError(throwError))
import           Data.Habulara.Internal.Commons.Convert
                 ( bsFromDay
                 , bsFromDecimal
                 , bsFromInteger
                 , dayFromInteger
                 , integerFromDay
                 , integerFromScientific
                 , textFromDay
                 , textFromDecimal
                 , textFromInteger
                 )
import           Data.Habulara.Internal.Commons.Read    (readHMB, readHMT)
import           Data.Habulara.Types
                 ( Value(VBoolean, VDate, VDecimal, VEmpty, VInt, VRaw, VText)
                 , ValueOperator
                 )
import qualified Data.Text.Encoding                     as TE


vEmpty :: ValueOperator
vEmpty _ = pure VEmpty


vRaw :: ValueOperator
vRaw VEmpty           = pure VEmpty
vRaw x@(VRaw _)       = pure x
vRaw (VInt x)         = VRaw <$> bsFromInteger x
vRaw (VText x)        = pure $ VRaw (TE.encodeUtf8 x)
vRaw (VDecimal x)     = VRaw <$> bsFromDecimal x
vRaw (VBoolean False) = pure $ VRaw "False"
vRaw (VBoolean True)  = pure $ VRaw "True"
vRaw (VDate x)        = VRaw <$> bsFromDay x


vInt :: ValueOperator
vInt VEmpty           = pure VEmpty
vInt (VRaw x)         = VInt <$> readHMB x
vInt x@(VInt _)       = pure x
vInt (VText x)        = VInt <$> readHMT x
vInt (VDecimal x)     = VInt <$> integerFromScientific x
vInt (VBoolean False) = pure $ VInt 0
vInt (VBoolean True)  = pure $ VInt 1
vInt (VDate x)        = VInt <$> integerFromDay x


vText :: ValueOperator
vText VEmpty           = pure VEmpty
vText (VRaw x)         = pure $ VText (TE.decodeUtf8 x)
vText (VInt x)         = VText <$> textFromInteger x
vText x@(VText _)      = pure x
vText (VDecimal x)     = VText <$> textFromDecimal x
vText (VBoolean False) = pure $ VText "False"
vText (VBoolean True)  = pure $ VText "True"
vText (VDate x)        = VText <$> textFromDay x


vDecimal :: ValueOperator
vDecimal VEmpty           = pure VEmpty
vDecimal (VRaw x)         = VDecimal <$> readHMB x
vDecimal (VInt x)         = pure (VDecimal $ fromInteger x)
vDecimal (VText x)        = VDecimal <$> readHMT x
vDecimal x@(VDecimal _)   = pure x
vDecimal (VBoolean False) = pure $ VDecimal 0
vDecimal (VBoolean True)  = pure $ VDecimal 1
vDecimal (VDate x)        = VInt <$> integerFromDay x


vBoolean :: ValueOperator
vBoolean VEmpty         = pure VEmpty
vBoolean (VRaw x)       = VBoolean <$> readHMB x
vBoolean (VInt x)       = pure $ VBoolean (x /= 0)
vBoolean (VText x)      = VBoolean <$> readHMT x
vBoolean (VDecimal x)   = pure $ VBoolean (x /= 0)
vBoolean x@(VBoolean _) = pure x
vBoolean (VDate _)      = pure $ VBoolean True


vDate :: ValueOperator
vDate VEmpty       = pure VEmpty
vDate (VRaw x)     = VDate <$> readHMB x
vDate (VInt x)     = VDate <$> dayFromInteger x
vDate (VText x)    = VDate <$> readHMT x
vDate (VDecimal x) = VDate <$> (dayFromInteger =<< integerFromScientific x)
vDate (VBoolean _) = throwError "Can not create date from boolean."
vDate x@(VDate _)  = pure x
