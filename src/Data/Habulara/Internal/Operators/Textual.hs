{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Internal.Operators.Textual where

import           Control.Monad.Except                   (MonadError(throwError))
import qualified Data.Habulara.Internal.Commons.Text    as CT
import           Data.Habulara.Internal.ValueMapping    (vRaw, vText)
import           Data.Habulara.Internal.ValuePrimitives (sanitizedText, trimmedText)
import           Data.Habulara.Types                    (Value(..), ValueOperator)
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

