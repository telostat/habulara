{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Internal.Operators.Boolean where

import           Control.Monad.Except                     (MonadError(throwError))
import           Data.Habulara.Internal.Operators.Textual (upper)
import           Data.Habulara.Internal.ValueMapping      (vRaw, vText)
import           Data.Habulara.Types                      (HabularaErrorM, Value(..), ValueOperator)
import qualified Data.Text                                as T


-- >>> booleanMap "Yes" "No" $ text "Yes" :: Either String Value
-- Right (VBoolean True)
-- >>> booleanMap "Yes" "No" $ text "No" :: Either String Value
-- Right (VBoolean False)
-- >>> booleanMap "Yes" "No" $ text "YES" :: Either String Value
-- Left "Unkown value for boolean map: YES"
-- >>> booleanMap "Yes" "No" $ text "HEBELE" :: Either String Value
-- Left "Unkown value for boolean map: HEBELE"
booleanMap :: T.Text -> T.Text -> ValueOperator
booleanMap _ _ VEmpty     = pure VEmpty
booleanMap t f (VText x)  = VBoolean <$> checkBooleanMap t f x
booleanMap t f x@(VRaw _) = vText x >>= booleanMap t f >>= vRaw
booleanMap _ _ _          = throwError "Operator can only be applied to values of textual types."


-- >>> booleanMapCI "Yes" "No" $ text "Yes" :: Either String Value
-- Right (VBoolean True)
-- >>> booleanMapCI "Yes" "No" $ text "No" :: Either String Value
-- Right (VBoolean False)
-- >>> booleanMapCI "Yes" "No" $ text "YES" :: Either String Value
-- Right (VBoolean True)
-- >>> booleanMapCI "Yes" "No" $ text "Hebele" :: Either String Value
-- Left "Unkown value for boolean map: HEBELE"
booleanMapCI :: T.Text -> T.Text -> ValueOperator
booleanMapCI _ _ VEmpty      = pure VEmpty
booleanMapCI t f x@(VText _) = upper x >>= booleanMap (T.toUpper t) (T.toUpper f)
booleanMapCI t f x@(VRaw _)  = vText x >>= booleanMapCI t f >>= vRaw
booleanMapCI _ _ _           = throwError "Operator can only be applied to values of textual types."


checkBooleanMap :: HabularaErrorM m => T.Text -> T.Text -> T.Text -> m Bool
checkBooleanMap t _ x | x == t = pure True
checkBooleanMap _ f x | x == f = pure False
checkBooleanMap _ _ x = throwError $ "Unkown value for boolean map: " <> T.unpack x
