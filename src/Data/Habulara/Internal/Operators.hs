{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Habulara.Internal.Operators where

import           Control.Monad.Except                   (MonadError(throwError))
import qualified Data.Habulara.Internal.Commons.Convert as CC
import qualified Data.Habulara.Internal.Commons.Read    as CR
import qualified Data.Habulara.Internal.Commons.Text    as CT
import           Data.Habulara.Types                    (HabularaErrorM, Value(..), ValueOperator)
import           Data.Scientific                        (Scientific)
import qualified Data.Set                               as S
import qualified Data.Text                              as T
import           Data.Time                              (Day, LocalTime(..), midnight)


-- ** Value Primitives


empty :: Value
empty = VEmpty


int :: Integer -> Value
int = VInt


text :: T.Text -> Value
text x
  | T.null x = VEmpty
  | otherwise = VText x


trimmedText :: T.Text -> Value
trimmedText x = maybe VEmpty VText (CT.nonEmptyText $ T.strip x)


sanitizedText :: T.Text -> Value
sanitizedText x = maybe VEmpty VText (CT.nonEmptySanitizedText x)


decimal :: Scientific -> Value
decimal = VDecimal


boolean :: Bool -> Value
boolean = VBoolean


true :: Value
true = boolean True


false :: Value
false = boolean False


date :: Day -> Value
date = VDate


datetime :: LocalTime -> Value
datetime = VDateTime


-- ** Value Conversions


vEmpty :: ValueOperator
vEmpty _ = pure VEmpty


vInt :: ValueOperator
vInt VEmpty           = pure VEmpty
vInt x@(VInt _)       = pure x
vInt (VText x)        = VInt <$> CR.readHMT x
vInt (VDecimal x)     = VInt <$> CC.integerFromScientific x
vInt (VBoolean False) = pure $ VInt 0
vInt (VBoolean True)  = pure $ VInt 1
vInt (VDate x)        = VInt <$> CC.integerFromDay x
vInt (VDateTime x)    = VInt <$> CC.integerFromLocalTime x


vText :: ValueOperator
vText VEmpty           = pure VEmpty
vText (VInt x)         = VText <$> CC.textFromInteger x
vText x@(VText _)      = pure x
vText (VDecimal x)     = VText <$> CC.textFromDecimal x
vText (VBoolean False) = pure $ VText "False"
vText (VBoolean True)  = pure $ VText "True"
vText (VDate x)        = VText <$> CC.textFromDay x
vText (VDateTime x)    = VText <$> CC.textFromLocalTime x


vDecimal :: ValueOperator
vDecimal VEmpty           = pure VEmpty
vDecimal (VInt x)         = pure (VDecimal $ fromInteger x)
vDecimal (VText x)        = VDecimal <$> CR.readHMT x
vDecimal x@(VDecimal _)   = pure x
vDecimal (VBoolean False) = pure $ VDecimal 0
vDecimal (VBoolean True)  = pure $ VDecimal 1
vDecimal (VDate x)        = VDecimal . fromInteger <$> CC.integerFromDay x
vDecimal (VDateTime x)    = VDecimal . fromRational <$> CC.rationalFromLocalTime x


vBoolean :: ValueOperator
vBoolean VEmpty         = pure VEmpty
vBoolean (VInt x)       = pure $ VBoolean (x /= 0)
vBoolean (VText x)      = VBoolean <$> CR.readHMT x
vBoolean (VDecimal x)   = pure $ VBoolean (x /= 0)
vBoolean x@(VBoolean _) = pure x
vBoolean (VDate _)      = pure $ VBoolean True
vBoolean (VDateTime _)  = pure $ VBoolean True


vDate :: ValueOperator
vDate VEmpty        = pure VEmpty
vDate (VInt x)      = VDate <$> CC.dayFromInteger x
vDate (VText x)     = VDate <$> CR.readHMT x
vDate (VDecimal x)  = VDate <$> (CC.dayFromInteger =<< CC.integerFromScientific x)
vDate (VBoolean _)  = throwError "Can not create date from boolean."
vDate x@(VDate _)   = pure x
vDate (VDateTime x) = pure $ VDate (localDay x)


vDateTime :: ValueOperator
vDateTime VEmpty          = pure VEmpty
vDateTime (VInt x)        = VDateTime <$> CC.dateTimeFromRational (toRational x)
vDateTime (VText x)       = VDateTime <$> CR.readHMT x
vDateTime (VDecimal x)    = VDateTime <$> CC.dateTimeFromRational (toRational x)
vDateTime (VBoolean _)    = throwError "Can not create date/time from boolean."
vDateTime (VDate x)       = pure $ VDateTime (LocalTime x midnight)
vDateTime x@(VDateTime _) = pure x


-- ** Text Operators


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
trim VEmpty    = pure VEmpty
trim (VText x) = pure $ trimmedText x
trim _         = throwError "Operator can only be applied to values of textual types."


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
sanitize VEmpty    = pure VEmpty
sanitize (VText x) = pure $ sanitizedText x
sanitize _         = throwError "Operator can only be applied to values of textual types."


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
lower VEmpty    = pure VEmpty
lower (VText x) = pure $ VText (T.toLower x)
lower _         = throwError "Operator can only be applied to values of textual types."


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
upper VEmpty    = pure VEmpty
upper (VText x) = pure $ VText (T.toUpper x)
upper _         = throwError "Operator can only be applied to values of textual types."


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
capitalize VEmpty    = pure VEmpty
capitalize (VText x) = pure $ VText (CT.capitalize x)
capitalize _         = throwError "Operator can only be applied to values of textual types."


-- >>> append "a" $ text "" :: Either String Value
-- Right VEmpty
-- >>> append "a" $ text "x" :: Either String Value
-- Right (VText "xa")
-- >>> append "a" $ text "x " :: Either String Value
-- Right (VText "x a")
append :: T.Text -> ValueOperator
append _ VEmpty    = pure VEmpty
append s (VText x) = pure $ VText (x <> s)
append _ _         = throwError "Operator can only be applied to values of textual types."


-- >>> prepend "a" $ text "" :: Either String Value
-- Right VEmpty
-- >>> prepend "a" $ text "x" :: Either String Value
-- Right (VText "ax")
-- >>> prepend "a" $ text "x " :: Either String Value
-- Right (VText "ax ")
prepend :: T.Text -> ValueOperator
prepend _ VEmpty    = pure VEmpty
prepend p (VText x) = pure $ VText (p <> x)
prepend _ _         = throwError "Operator can only be applied to values of textual types."


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
splitHead _ VEmpty    = pure VEmpty
splitHead s (VText x) = pure $ text $ T.takeWhile (/= s) x
splitHead _ _         = throwError "Operator can only be applied to values of textual types."


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
splitTail _ VEmpty    = pure VEmpty
splitTail s (VText x) = pure $ text $ T.drop 1 $ T.dropWhile (/= s) x
splitTail _ _         = throwError "Operator can only be applied to values of textual types."


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
oneOfText _ _           = throwError "Operator can only be applied to values of textual types."


checkTextSet :: HabularaErrorM m => S.Set T.Text -> T.Text -> m ()
checkTextSet s x
  | S.member x s = pure ()
  | otherwise    = throwError $ "Unrecognized value for set: " <> T.unpack x


constant :: T.Text -> ValueOperator
constant _ VEmpty      = pure VEmpty
constant s x@(VText v) = x <$ checkConstant s v
constant _ _           = throwError "Operator can only be applied to values of textual types."


checkConstant :: HabularaErrorM m => T.Text -> T.Text -> m ()
checkConstant e x
  | e == x = pure ()
  | otherwise = throwError $ "Unexpected value: " <> T.unpack x


constantEmpty :: ValueOperator
constantEmpty VEmpty = pure VEmpty
constantEmpty x      = throwError $ "Encountered value while expecting nothing: " <> show x


-- ** Date/Time Operators

-- >>> trim $ raw " a " :: Either String Value
-- Right (VRaw "a")
parseDate :: String -> ValueOperator
parseDate _ VEmpty      = pure VEmpty
parseDate fmt (VText x) = VDate <$> CC.parseDateFromString fmt (T.unpack x)
parseDate _ _           = throwError "Operator can only be applied to values of textual types."


-- >>> trim $ raw " a " :: Either String Value
-- Right (VRaw "a")
parseDateTime :: String -> ValueOperator
parseDateTime _ VEmpty      = pure VEmpty
parseDateTime fmt (VText x) = VDateTime <$> CC.parseLocalTimeFromString fmt (T.unpack x)
parseDateTime _ _           = throwError "Operator can only be applied to values of textual types."

-- ** Boolean Operators


-- >>> booleanMap "Yes" "No" $ text "Yes" :: Either String Value
-- Right (VBoolean True)
-- >>> booleanMap "Yes" "No" $ text "No" :: Either String Value
-- Right (VBoolean False)
-- >>> booleanMap "Yes" "No" $ text "YES" :: Either String Value
-- Left "Unkown value for boolean map: YES"
-- >>> booleanMap "Yes" "No" $ text "HEBELE" :: Either String Value
-- Left "Unkown value for boolean map: HEBELE"
booleanMap :: T.Text -> T.Text -> ValueOperator
booleanMap _ _ VEmpty    = pure VEmpty
booleanMap t f (VText x) = VBoolean <$> checkBooleanMap t f x
booleanMap _ _ _         = throwError "Operator can only be applied to values of textual types."


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
booleanMapCI _ _ _           = throwError "Operator can only be applied to values of textual types."


checkBooleanMap :: HabularaErrorM m => T.Text -> T.Text -> T.Text -> m Bool
checkBooleanMap t _ x | x == t = pure True
checkBooleanMap _ f x | x == f = pure False
checkBooleanMap _ _ x = throwError $ "Unkown value for boolean map: " <> T.unpack x


-- ** Numeric Operators


add :: Scientific -> ValueOperator
add addend (VDecimal x) = pure $ VDecimal (x + addend)
add _ _                 = throwError "Operator can only be applied to values of Decimal type."


sub :: Scientific -> ValueOperator
sub subtrahend (VDecimal x) = pure $ VDecimal (x - subtrahend)
sub _ _                     = throwError "Operator can only be applied to values of Decimal type."


subFrom :: Scientific -> ValueOperator
subFrom minuend (VDecimal x) = pure $ VDecimal (minuend - x)
subFrom _ _                  = throwError "Operator can only be applied to values of Decimal type."


multiply :: Scientific -> ValueOperator
multiply factor (VDecimal x) = pure $ VDecimal (x * factor)
multiply _ _                 = throwError "Operator can only be applied to values of Decimal type."


divide :: Scientific -> ValueOperator
divide dividend (VDecimal x) = pure $ VDecimal (dividend / x)  -- TODO: Encode the possibility of division errors.
divide _ _                   = throwError "Operator can only be applied to values of Decimal type."


divideBy :: Scientific -> ValueOperator
divideBy divisor (VDecimal x) = pure $ VDecimal (x / divisor)  -- TODO: Encode the possibility of division errors.
divideBy _ _                  = throwError "Operator can only be applied to values of Decimal type."


percentage :: ValueOperator
percentage = multiply 100
