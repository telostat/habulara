{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Internal.Operators.Textual where

import           Control.Monad.Except                   (MonadError(throwError))
import qualified Data.Habulara.Internal.Commons.Text    as CT
import           Data.Habulara.Internal.ValueMapping    (vRaw, vText)
import           Data.Habulara.Internal.ValuePrimitives (sanitizedText, text, trimmedText)
import           Data.Habulara.Types                    (HabularaErrorM, Value(..), ValueOperator)
import qualified Data.Set                               as S
import qualified Data.Text                              as T


-- >>> trim $ text "" :: Either String Value
-- Right VEmpty
-- >>> trim $ text " " :: Either String Value
-- Right VEmpty
-- >>> trim $ text "a" :: Either String Value
-- Right (VText "a")
-- >>> trim $ text " a " :: Either String Value
-- Right (VText "a")
-- >>> trim $ raw "" :: Either String Value
-- Right VEmpty
-- >>> trim $ raw " " :: Either String Value
-- Right VEmpty
-- >>> trim $ raw "a" :: Either String Value
-- Right (VRaw "a")
-- >>> trim $ raw " a " :: Either String Value
-- Right (VRaw "a")
trim :: ValueOperator
trim VEmpty     = pure VEmpty
trim (VText x)  = pure $ trimmedText x
trim x@(VRaw _) = vText x >>= trim >>= vRaw
trim _          = throwError "Operator can only be applied to values of textual types."


-- >>> sanitize $ text "" :: Either String Value
-- Right VEmpty
-- >>> sanitize $ text " " :: Either String Value
-- Right VEmpty
-- >>> sanitize $ text "a" :: Either String Value
-- Right (VText "a")
-- >>> sanitize $ text " a " :: Either String Value
-- Right (VText "a")
-- >>> sanitize $ text " a \t\r\n b" :: Either String Value
-- Right (VText "a b")
sanitize :: ValueOperator
sanitize VEmpty     = pure VEmpty
sanitize (VText x)  = pure $ sanitizedText x
sanitize x@(VRaw _) = vText x >>= sanitize >>= vRaw
sanitize _          = throwError "Operator can only be applied to values of textual types."


-- >>> lower $ text "" :: Either String Value
-- Right VEmpty
-- >>> lower $ text " " :: Either String Value
-- Right (VText " ")
-- >>> lower $ text "A" :: Either String Value
-- Right (VText "a")
-- >>> lower $ text "A" :: Either String Value
-- Right (VText "a")
-- >>> lower $ raw "" :: Either String Value
-- Right VEmpty
-- >>> lower $ raw " " :: Either String Value
-- Right (VRaw " ")
-- >>> lower $ raw "A" :: Either String Value
-- Right (VRaw "a")
-- >>> lower $ raw "A" :: Either String Value
-- Right (VRaw "AA")
lower :: ValueOperator
lower VEmpty     = pure VEmpty
lower (VText x)  = pure $ VText (T.toLower x)
lower x@(VRaw _) = vText x >>= lower >>= vRaw
lower _          = throwError "Operator can only be applied to values of textual types."


-- >>> upper $ text "" :: Either String Value
-- Right VEmpty
-- >>> upper $ text " " :: Either String Value
-- Right (VText " ")
-- >>> upper $ text "a" :: Either String Value
-- Right (VText "A")
-- >>> upper $ text "aa" :: Either String Value
-- Right (VText "AA")
-- >>> upper $ raw "" :: Either String Value
-- Right VEmpty
-- >>> upper $ raw " " :: Either String Value
-- Right (VRaw " ")
-- >>> upper $ raw "a" :: Either String Value
-- Right (VRaw "A")
-- >>> upper $ raw "aa" :: Either String Value
-- Right (VRaw "AA")
upper :: ValueOperator
upper VEmpty     = pure VEmpty
upper (VText x)  = pure $ VText (T.toUpper x)
upper x@(VRaw _) = vText x >>= upper >>= vRaw
upper _          = throwError "Operator can only be applied to values of textual types."


-- >>> capitalize $ text "" :: Either String Value
-- Right VEmpty
-- >>> capitalize $ text " " :: Either String Value
-- Right (VText " ")
-- >>> capitalize $ text "a" :: Either String Value
-- Right (VText "A")
-- >>> capitalize $ text "aa" :: Either String Value
-- Right (VText "Aa")
-- >>> capitalize $ raw "" :: Either String Value
-- Right VEmpty
-- >>> capitalize $ raw " " :: Either String Value
-- Right (VRaw " ")
-- >>> capitalize $ raw "a" :: Either String Value
-- Right (VRaw "A")
-- >>> capitalize $ raw "aa" :: Either String Value
-- Right (VRaw "Aa")
capitalize :: ValueOperator
capitalize VEmpty     = pure VEmpty
capitalize (VText x)  = pure $ VText (CT.capitalize x)
capitalize x@(VRaw _) = vText x >>= capitalize >>= vRaw
capitalize _          = throwError "Operator can only be applied to values of textual types."


-- >>> append "a" $ text "" :: Either String Value
-- Right VEmpty
-- >>> append "a" $ text "x" :: Either String Value
-- Right (VText "xa")
-- >>> append "a" $ text "x " :: Either String Value
-- Right (VText "x a")
append :: T.Text -> ValueOperator
append _ VEmpty     = pure VEmpty
append s (VText x)  = pure $ VText (x <> s)
append s x@(VRaw _) = vText x >>= append s >>= vRaw
append _ _          = throwError "Operator can only be applied to values of textual types."


-- >>> prepend "a" $ text "" :: Either String Value
-- Right VEmpty
-- >>> prepend "a" $ text "x" :: Either String Value
-- Right (VText "ax")
-- >>> prepend "a" $ text "x " :: Either String Value
-- Right (VText "ax ")
prepend :: T.Text -> ValueOperator
prepend _ VEmpty     = pure VEmpty
prepend p (VText x)  = pure $ VText (p <> x)
prepend p x@(VRaw _) = vText x >>= prepend p >>= vRaw
prepend _ _          = throwError "Operator can only be applied to values of textual types."


-- >>> splitHead '/' $ text "" :: Either String Value
-- Right VEmpty
-- >>> splitHead '/' $ text "a" :: Either String Value
-- Right (VText "a")
-- >>> splitHead '/' $ text "a/" :: Either String Value
-- Right (VText "a")
-- >>> splitHead '/' $ text "/b" :: Either String Value
-- Right VEmpty
-- >>> splitHead '/' $ text "a/b" :: Either String Value
-- Right (VText "a")
-- >>> splitHead '/' $ text "a/b/" :: Either String Value
-- Right (VText "a")
-- >>> splitHead '/' $ text "a/b/c" :: Either String Value
-- Right (VText "a")
splitHead :: Char -> ValueOperator
splitHead _ VEmpty     = pure VEmpty
splitHead s (VText x)  = pure $ text $ T.takeWhile (/= s) x
splitHead s x@(VRaw _) = vText x >>= splitHead s >>= vRaw
splitHead _ _          = throwError "Operator can only be applied to values of textual types."


-- >>> splitTail '/' $ text "" :: Either String Value
-- Right VEmpty
-- >>> splitTail '/' $ text "a" :: Either String Value
-- Right VEmpty
-- >>> splitTail '/' $ text "a/" :: Either String Value
-- Right VEmpty
-- >>> splitTail '/' $ text "/b" :: Either String Value
-- Right (VText "b")
-- >>> splitTail '/' $ text "a/b" :: Either String Value
-- Right (VText "b")
-- >>> splitTail '/' $ text "a/b/" :: Either String Value
-- Right (VText "b/")
-- >>> splitTail '/' $ text "a/b/c" :: Either String Value
-- Right (VText "b/c")
splitTail :: Char -> ValueOperator
splitTail _ VEmpty     = pure VEmpty
splitTail s (VText x)  = pure $ text $ T.drop 1 $ T.dropWhile (/= s) x
splitTail s x@(VRaw _) = vText x >>= splitTail s >>= vRaw
splitTail _ _          = throwError "Operator can only be applied to values of textual types."


-- >>> oneOfText (S.fromList ["A", "B", "C"]) $ text "" :: Either String Value
-- Right VEmpty
-- >>> oneOfText (S.fromList ["A", "B", "C"]) $ text "A" :: Either String Value
-- Right (VText "A")
-- >>> oneOfText (S.fromList ["A", "B", "C"]) $ text "a" :: Either String Value
-- Left "Unrecognized value for set: a"
-- >>> oneOfText (S.fromList ["A", "B", "C"]) $ text "x" :: Either String Value
-- Left "Unrecognized value for set: x"
oneOfText :: S.Set T.Text -> ValueOperator
oneOfText _ VEmpty      = pure VEmpty
oneOfText s x@(VText v) = x <$ checkTextSet s v
oneOfText s x@(VRaw _)  = vText x >>= oneOfText s >>= vRaw
oneOfText _ _           = throwError "Operator can only be applied to values of textual types."


checkTextSet :: HabularaErrorM m => S.Set T.Text -> T.Text -> m ()
checkTextSet s x
  | S.member x s = pure ()
  | otherwise    = throwError $ "Unrecognized value for set: " <> T.unpack x


constant :: T.Text -> ValueOperator
constant _ VEmpty      = pure VEmpty
constant s x@(VText v) = x <$ checkConstant s v
constant s x@(VRaw _)  = vText x >>= constant s >>= vRaw
constant _ _           = throwError "Operator can only be applied to values of textual types."


checkConstant :: HabularaErrorM m => T.Text -> T.Text -> m ()
checkConstant e x
  | e == x = pure ()
  | otherwise = throwError $ "Unexpected value: " <> T.unpack x


constantEmpty :: ValueOperator
constantEmpty VEmpty = pure VEmpty
constantEmpty x      = throwError $ "Encountered value while expecting nothing: " <> show x
