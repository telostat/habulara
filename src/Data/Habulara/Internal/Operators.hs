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


-- | Builds 'VEmpty' value.
--
-- >>> empty
-- VEmpty
-- >>> :type empty
-- empty :: Value
empty :: Value
empty = VEmpty


-- | Builds a 'VInt' value.
--
-- >>> int 42
-- VInt 42
int :: Integer -> Value
int = VInt


-- | Attempts to builds a 'VText' value.
--
-- If the argument is empty text, the return value is 'VEmpty'.
--
-- >>> text ""
-- VEmpty
-- >>> text " "
-- VText " "
-- >>> text "42"
-- VText "42"
-- >>> text " 42 "
-- VText " 42 "
text :: T.Text -> Value
text x
  | T.null x = VEmpty
  | otherwise = VText x


-- | Attempts to builds a 'VText' value.
--
-- All whitespace is trimmed from the argument.
--
-- >>> trimmedText ""
-- VEmpty
-- >>> trimmedText " "
-- VEmpty
-- >>> trimmedText "42"
-- VText "42"
-- >>> trimmedText " 42 "
-- VText "42"
-- >>> trimmedText " \t\r\n42 \t\r\n"
-- VText "42"
trimmedText :: T.Text -> Value
trimmedText x = maybe VEmpty VText (CT.nonEmptyText $ T.strip x)


-- | Attempts to builds a 'VText' value.
--
-- All whitespace is trimmed and sanitized in the given text.
--
-- >>> sanitizedText ""
-- VEmpty
-- >>> sanitizedText " "
-- VEmpty
-- >>> sanitizedText "42"
-- VText "42"
-- >>> sanitizedText " 42 "
-- VText "42"
-- >>> sanitizedText " \t\r\n42 \t\r\n"
-- VText "42"
-- >>> sanitizedText " \t\r\n42 \t\r\nis \t\r\na \t\r\nnumber \t\r\n"
-- VText "42 is a number"
sanitizedText :: T.Text -> Value
sanitizedText x = maybe VEmpty VText (CT.nonEmptySanitizedText x)


-- | Builds a 'VDecimal' value.
--
-- >>> decimal 42
-- VDecimal 42.0
decimal :: Scientific -> Value
decimal = VDecimal


-- | Builds a 'VBoolean' value.
--
-- >>> boolean True
-- VBoolean True
-- >>> boolean False
-- VBoolean False
boolean :: Bool -> Value
boolean = VBoolean


-- | Represents a true 'VBoolean' value.
--
-- >>> true
-- VBoolean True
true :: Value
true = boolean True


-- | Represents a false 'VBoolean' value.
--
-- >>> false
-- VBoolean False
false :: Value
false = boolean False


-- | Builds a 'VDate' value.
--
-- >>> date $ read "2020-12-31"
-- VDate 2020-12-31
date :: Day -> Value
date = VDate


-- | Builds a 'VDateTime' value.
--
-- >>> datetime $ read "2020-12-31 23:59:59"
-- VDateTime 2020-12-31 23:59:59
datetime :: LocalTime -> Value
datetime = VDateTime


-- ** Value Conversions


-- | Returns a 'VEmpty' value regardless of the argument.
--
-- >>> vEmpty VEmpty :: Either String Value
-- Right VEmpty
-- >>> vEmpty $ int 42 :: Either String Value
-- Right VEmpty
vEmpty :: ValueOperator
vEmpty = pure . const VEmpty


-- | Attempts to convert the given 'Value' to a 'VInt' value.
--
-- Conversion rules:
--
-- - 'VBoolean' values are converted as @0@ or @1@ for 'False' and 'True' values
--   respectively.
-- - 'VDate' values are converted to 'VInt' values as a Julian date.
-- - 'VDateTime' values are converted to 'VInt' values as UNIX timestamps.
--
-- See below examples for conversations.
--
-- >>> vInt VEmpty :: Either String Value
-- Right VEmpty
-- >>> vInt $ int 42 :: Either String Value
-- Right (VInt 42)
-- >>> vInt $ text "42" :: Either String Value
-- Right (VInt 42)
-- >>> vInt $ decimal 42 :: Either String Value
-- Right (VInt 42)
-- >>> vInt $ true :: Either String Value
-- Right (VInt 1)
-- >>> vInt $ false :: Either String Value
-- Right (VInt 0)
-- >>> vInt $ date $ read "2020-12-31" :: Either String Value
-- Right (VInt 59214)
-- >>> vInt $ datetime $ read "2020-12-31 23:59:59" :: Either String Value
-- Right (VInt 1609459199)
vInt :: ValueOperator
vInt VEmpty           = pure VEmpty
vInt x@(VInt _)       = pure x
vInt (VText x)        = VInt <$> CR.readHMT x
vInt (VDecimal x)     = VInt <$> CC.integerFromScientific x
vInt (VBoolean False) = pure $ VInt 0
vInt (VBoolean True)  = pure $ VInt 1
vInt (VDate x)        = VInt <$> CC.integerFromDay x
vInt (VDateTime x)    = VInt <$> CC.integerFromLocalTime x


-- | Attempts to convert the given 'Value' to a 'VText' value.
--
-- See below examples for conversations.
--
-- >>> vText VEmpty :: Either String Value
-- Right VEmpty
-- >>> vText $ int 42 :: Either String Value
-- Right (VText "42")
-- >>> vText $ text "a" :: Either String Value
-- Right (VText "a")
-- >>> vText $ decimal 42 :: Either String Value
-- Right (VText "42.0")
-- >>> vText $ true :: Either String Value
-- Right (VText "True")
-- >>> vText $ false :: Either String Value
-- Right (VText "False")
-- >>> vText $ date $ read "2020-12-31" :: Either String Value
-- Right (VText "2020-12-31")
-- >>> vText $ datetime $ read "2020-12-31 23:59:59" :: Either String Value
-- Right (VText "2020-12-31 23:59:59")
vText :: ValueOperator
vText VEmpty           = pure VEmpty
vText (VInt x)         = VText <$> CC.textFromInteger x
vText x@(VText _)      = pure x
vText (VDecimal x)     = VText <$> CC.textFromDecimal x
vText (VBoolean False) = pure $ VText "False"
vText (VBoolean True)  = pure $ VText "True"
vText (VDate x)        = VText <$> CC.textFromDay x
vText (VDateTime x)    = VText <$> CC.textFromLocalTime x


-- | Attempts to convert the given 'Value' to a 'VDecimal' value.
--
-- See below examples for conversations.
--
-- >>> vDecimal VEmpty :: Either String Value
-- Right VEmpty
-- >>> vDecimal $ int 42 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> vDecimal $ text "a" :: Either String Value
-- Left "Can not convert from value. Value was: a"
-- >>> vDecimal $ text "42" :: Either String Value
-- Right (VDecimal 42.0)
-- >>> vDecimal $ decimal 42 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> vDecimal $ true :: Either String Value
-- Right (VDecimal 1.0)
-- >>> vDecimal $ false :: Either String Value
-- Right (VDecimal 0.0)
-- >>> vDecimal $ date $ read "2020-12-31" :: Either String Value
-- Right (VDecimal 59214.0)
-- >>> vDecimal $ datetime $ read "2020-12-31 23:59:59" :: Either String Value
-- Right (VDecimal 1.609459199e9)
vDecimal :: ValueOperator
vDecimal VEmpty           = pure VEmpty
vDecimal (VInt x)         = pure (VDecimal $ fromInteger x)
vDecimal (VText x)        = VDecimal <$> CR.readHMT x
vDecimal x@(VDecimal _)   = pure x
vDecimal (VBoolean False) = pure $ VDecimal 0
vDecimal (VBoolean True)  = pure $ VDecimal 1
vDecimal (VDate x)        = VDecimal . fromInteger <$> CC.integerFromDay x
vDecimal (VDateTime x)    = VDecimal . fromRational <$> CC.rationalFromLocalTime x


-- | Attempts to convert the given 'Value' to a 'VBoolean' value.
--
-- See below examples for conversations.
--
-- >>> vBoolean VEmpty :: Either String Value
-- Right VEmpty
-- >>> vBoolean $ int 0 :: Either String Value
-- Right (VBoolean False)
-- >>> vBoolean $ int 1 :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ int 42 :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ int (-42) :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ text "a" :: Either String Value
-- Left "Can not convert from value. Value was: A"
-- >>> vBoolean $ text "True" :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ text "False" :: Either String Value
-- Right (VBoolean False)
-- >>> vBoolean $ text "TRUE" :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ text "FALSE" :: Either String Value
-- Right (VBoolean False)
-- >>> vBoolean $ decimal 42 :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ true :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ false :: Either String Value
-- Right (VBoolean False)
-- >>> vBoolean $ date $ read "2020-12-31" :: Either String Value
-- Right (VBoolean True)
-- >>> vBoolean $ datetime $ read "2020-12-31 23:59:59" :: Either String Value
-- Right (VBoolean True)
vBoolean :: ValueOperator
vBoolean VEmpty         = pure VEmpty
vBoolean (VInt x)       = pure $ VBoolean (x /= 0)
vBoolean (VText x)      = VBoolean <$> CR.readHMT (CT.capitalize x)
vBoolean (VDecimal x)   = pure $ VBoolean (x /= 0)
vBoolean x@(VBoolean _) = pure x
vBoolean (VDate _)      = pure $ VBoolean True
vBoolean (VDateTime _)  = pure $ VBoolean True


-- | Attempts to convert the given 'Value' to a 'VDate' value.
--
-- See below examples for conversations. Some connversations are not possible by definition.
--
-- >>> vDate VEmpty :: Either String Value
-- Right VEmpty
-- >>> vDate $ int 0 :: Either String Value
-- Right (VDate 1858-11-17)
-- >>> vDate $ int 1 :: Either String Value
-- Right (VDate 1858-11-18)
-- >>> vDate $ text "2020-12-31" :: Either String Value
-- Right (VDate 2020-12-31)
-- >>> vDate $ decimal 0 :: Either String Value
-- Right (VDate 1858-11-17)
-- >>> vDate $ decimal 1 :: Either String Value
-- Right (VDate 1858-11-18)
-- >>> vDate $ true :: Either String Value
-- Left "Can not create date from boolean."
-- >>> vDate $ false :: Either String Value
-- Left "Can not create date from boolean."
-- >>> vDate $ date $ read "2020-12-31" :: Either String Value
-- Right (VDate 2020-12-31)
-- >>> vDate $ datetime $ read "2020-12-31 23:59:59" :: Either String Value
-- Right (VDate 2020-12-31)
vDate :: ValueOperator
vDate VEmpty        = pure VEmpty
vDate (VInt x)      = VDate <$> CC.dayFromInteger x
vDate (VText x)     = VDate <$> CR.readHMT x
vDate (VDecimal x)  = VDate <$> (CC.dayFromInteger =<< CC.integerFromScientific x)
vDate (VBoolean _)  = throwError "Can not create date from boolean."
vDate x@(VDate _)   = pure x
vDate (VDateTime x) = pure $ VDate (localDay x)


-- | Attempts to convert the given 'Value' to a 'VDate' value.
--
-- See below examples for conversations. Some connversations are not possible by definition.
--
-- >>> vDateTime VEmpty :: Either String Value
-- Right VEmpty
-- >>> vDateTime $ int 0 :: Either String Value
-- Right (VDateTime 1970-01-01 00:00:00)
-- >>> vDateTime $ int 1 :: Either String Value
-- Right (VDateTime 1970-01-01 00:00:01)
-- >>> vDateTime $ text "2020-12-31 23:59:59" :: Either String Value
-- Right (VDateTime 2020-12-31 23:59:59)
-- >>> vDateTime $ decimal 0 :: Either String Value
-- Right (VDateTime 1970-01-01 00:00:00)
-- >>> vDateTime $ decimal 1 :: Either String Value
-- Right (VDateTime 1970-01-01 00:00:01)
-- >>> vDateTime $ decimal 1.1000001 :: Either String Value
-- Right (VDateTime 1970-01-01 00:00:01.1000001)
-- >>> vDateTime $ true :: Either String Value
-- Left "Can not create date/time from boolean."
-- >>> vDateTime $ false :: Either String Value
-- Left "Can not create date/time from boolean."
-- >>> vDateTime $ date $ read "2020-12-31" :: Either String Value
-- Right (VDateTime 2020-12-31 00:00:00)
-- >>> vDateTime $ datetime $ read "2020-12-31 23:59:59" :: Either String Value
-- Right (VDateTime 2020-12-31 23:59:59)
vDateTime :: ValueOperator
vDateTime VEmpty          = pure VEmpty
vDateTime (VInt x)        = VDateTime <$> CC.dateTimeFromRational (toRational x)
vDateTime (VText x)       = VDateTime <$> CR.readHMT x
vDateTime (VDecimal x)    = VDateTime <$> CC.dateTimeFromRational (toRational x)
vDateTime (VBoolean _)    = throwError "Can not create date/time from boolean."
vDateTime (VDate x)       = pure $ VDateTime (LocalTime x midnight)
vDateTime x@(VDateTime _) = pure x


-- ** Common Operators

-- | Attempts to ensure that the 'Value' is always 'VEmpty'.
--
-- >>> constantEmpty (text "") :: Either String Value
-- Right VEmpty
-- >>> constantEmpty (text "a") :: Either String Value
-- Left "Encountered value while expecting nothing: VText \"a\""
-- >>> constantEmpty (int 0) :: Either String Value
-- Left "Encountered value while expecting nothing: VInt 0"
constantEmpty :: ValueOperator
constantEmpty VEmpty = pure VEmpty
constantEmpty x      = throwError $ "Encountered value while expecting nothing: " <> show x


-- ** Text Operators


-- | Attempts to trims a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
-- >>> trim $ text "" :: Either String Value
-- Right VEmpty
-- >>> trim $ text " " :: Either String Value
-- Right VEmpty
-- >>> trim $ text "a" :: Either String Value
-- Right (VText "a")
-- >>> trim $ text " a " :: Either String Value
-- Right (VText "a")
trim :: ValueOperator
trim VEmpty    = pure VEmpty
trim (VText x) = pure $ trimmedText x
trim _         = throwError "Operator can only be applied to values of textual types."


-- | Attempts to sanitize a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
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


-- | Attempts to lowercase a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
-- >>> lower $ text "" :: Either String Value
-- Right VEmpty
-- >>> lower $ text " " :: Either String Value
-- Right (VText " ")
-- >>> lower $ text "a" :: Either String Value
-- Right (VText "a")
-- >>> lower $ text "A" :: Either String Value
-- Right (VText "a")
lower :: ValueOperator
lower VEmpty    = pure VEmpty
lower (VText x) = pure $ VText (T.toLower x)
lower _         = throwError "Operator can only be applied to values of textual types."


-- | Attempts to uppercase a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
-- >>> upper $ text "" :: Either String Value
-- Right VEmpty
-- >>> upper $ text " " :: Either String Value
-- Right (VText " ")
-- >>> upper $ text "a" :: Either String Value
-- Right (VText "A")
-- >>> upper $ text "aa" :: Either String Value
-- Right (VText "AA")
upper :: ValueOperator
upper VEmpty    = pure VEmpty
upper (VText x) = pure $ VText (T.toUpper x)
upper _         = throwError "Operator can only be applied to values of textual types."


-- | Attempts to capitalize a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
-- >>> capitalize $ text "" :: Either String Value
-- Right VEmpty
-- >>> capitalize $ text " " :: Either String Value
-- Right (VText " ")
-- >>> capitalize $ text "a" :: Either String Value
-- Right (VText "A")
-- >>> capitalize $ text "aa" :: Either String Value
-- Right (VText "Aa")
-- >>> capitalize $ text "aa aa" :: Either String Value
-- Right (VText "Aa aa")
-- >>> capitalize $ text "aA AA" :: Either String Value
-- Right (VText "Aa aa")
capitalize :: ValueOperator
capitalize VEmpty    = pure VEmpty
capitalize (VText x) = pure $ VText (CT.capitalize x)
capitalize _         = throwError "Operator can only be applied to values of textual types."


-- | Attempts to add a suffix to a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
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


-- | Attempts to add a prefix to a given textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
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


-- | Attempts to split the given text ONCE by the given character and returns the first part.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
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


-- | Attempts to split the given text ONCE by the given character and returns the second part.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
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


-- | Attempts to ensure that the given textual value is one of the given set of strings.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
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


-- | Attempts to ensure that the given textual value is always the same as the constant textual value.
--
-- This operator is applicable to 'VEmpty' and 'VText' values. The rest will result in error.
--
-- >>> constant "HEBELE" (text "HEBELE") :: Either String Value
-- Right (VText "HEBELE")
-- >>> constant "HUBELE" (text "HEBELE") :: Either String Value
-- Left "Unexpected value: HEBELE"
constant :: T.Text -> ValueOperator
constant _ VEmpty      = pure VEmpty
constant s x@(VText v) = x <$ checkConstant s v
constant _ _           = throwError "Operator can only be applied to values of textual types."


checkConstant :: HabularaErrorM m => T.Text -> T.Text -> m ()
checkConstant e x
  | e == x = pure ()
  | otherwise = throwError $ "Unexpected value: " <> T.unpack x


-- ** Date/Time Operators

-- | Attempts to parse a given textual value into a 'VDate' value.
--
-- >>> parseDate "%d-%m-%Y" $ text "31-12-2020" :: Either String Value
-- Right (VDate 2020-12-31)
-- >>> parseDate "%d-%m-%Y" $ text "31/12/2020" :: Either String Value
-- Left "Can not convert from value. Value was: \"31/12/2020\""
parseDate :: String -> ValueOperator
parseDate _ VEmpty      = pure VEmpty
parseDate fmt (VText x) = VDate <$> CC.parseDateFromString fmt (T.unpack x)
parseDate _ _           = throwError "Operator can only be applied to values of textual types."


-- | Attempts to parse a given textual value into a 'VDateTime' value.
--
-- >>> parseDateTime "%d-%m-%Y %H:%M:%S" $ text "31-12-2020 23:59:59" :: Either String Value
-- Right (VDateTime 2020-12-31 23:59:59)
-- >>> parseDateTime "%d-%m-%Y %H:%M:%S" $ text "31/12/2020 23:59:59" :: Either String Value
-- Left "Can not convert from value. Value was: \"31/12/2020 23:59:59\""
parseDateTime :: String -> ValueOperator
parseDateTime _ VEmpty      = pure VEmpty
parseDateTime fmt (VText x) = VDateTime <$> CC.parseLocalTimeFromString fmt (T.unpack x)
parseDateTime _ _           = throwError "Operator can only be applied to values of textual types."

-- ** Boolean Operators


-- | Attemps to produce a 'VBoolean' value as per given textual value via a simple map (case-sensitive).
--
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


-- | Attemps to produce a 'VBoolean' value as per given textual value via a
-- simple map (case-insensitive version of 'booleanMap').
--
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

-- | Performs addition operation on 'VDecimal' values.
--
-- >>> add 1 $ decimal 41 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> add 1 $ int 41 :: Either String Value
-- Left "Operator can only be applied to values of Decimal type."
add :: Scientific -> ValueOperator
add addend (VDecimal x) = pure $ VDecimal (x + addend)
add _ _                 = throwError "Operator can only be applied to values of Decimal type."


-- | Subtracts given subtrahend from a 'VDecimal' value (minuend).
--
-- >>> sub 1 $ decimal 43 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> sub 1 $ int 43 :: Either String Value
-- Left "Operator can only be applied to values of Decimal type."
sub :: Scientific -> ValueOperator
sub subtrahend (VDecimal x) = pure $ VDecimal (x - subtrahend)
sub _ _                     = throwError "Operator can only be applied to values of Decimal type."


-- | Subtracts a 'VDecimal' value (minuend) from a given given subtrahend.
--
-- >>> subFrom 43 $ decimal 1 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> subFrom 43 $ int 1 :: Either String Value
-- Left "Operator can only be applied to values of Decimal type."
subFrom :: Scientific -> ValueOperator
subFrom minuend (VDecimal x) = pure $ VDecimal (minuend - x)
subFrom _ _                  = throwError "Operator can only be applied to values of Decimal type."


-- | Performs multiplication operation on 'VDecimal' values.
--
-- >>> multiply 6 $ decimal 7 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> multiply 6 $ int 7 :: Either String Value
-- Left "Operator can only be applied to values of Decimal type."
multiply :: Scientific -> ValueOperator
multiply factor (VDecimal x) = pure $ VDecimal (x * factor)
multiply _ _                 = throwError "Operator can only be applied to values of Decimal type."


-- | Divides given dividend by a 'VDecimal' value (divisor).
--
-- >>> divide 84 $ decimal 2 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> divide 84 $ int 2 :: Either String Value
-- Left "Operator can only be applied to values of Decimal type."
divide :: Scientific -> ValueOperator
divide dividend (VDecimal x) = pure $ VDecimal (dividend / x)  -- TODO: Encode the possibility of division errors.
divide _ _                   = throwError "Operator can only be applied to values of Decimal type."


-- | Divides a 'VDecimal' value (dividend) by the given divisor.
--
-- >>> divideBy 2 $ decimal 84 :: Either String Value
-- Right (VDecimal 42.0)
-- >>> divideBy 2 $ int 84 :: Either String Value
-- Left "Operator can only be applied to values of Decimal type."
divideBy :: Scientific -> ValueOperator
divideBy divisor (VDecimal x) = pure $ VDecimal (x / divisor)  -- TODO: Encode the possibility of division errors.
divideBy _ _                  = throwError "Operator can only be applied to values of Decimal type."


-- | Returns the percentage points for the given 'VDecimal' value.
--
-- >>> percentage $ decimal 1 :: Either String Value
-- Right (VDecimal 100.0)
-- >>> percentage $ decimal 0.01 :: Either String Value
-- Right (VDecimal 1.0)
percentage :: ValueOperator
percentage = multiply 100
