{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Internal.Commons.Text where

import qualified Data.ByteString    as B
import           Data.Profunctor    (Profunctor(dimap))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE


-- | Type for non-empty 'T.Text' values.
newtype NonEmptyText = MkNonEmptyText { unNonEmptyText :: T.Text } deriving (Eq, Ord, Show)


-- | Safe constructor for 'NonEmptyText' values.
--
-- >>> mkNonEmptyText "" :: Maybe NonEmptyText
-- Nothing
-- >>> mkNonEmptyText " " :: Maybe NonEmptyText
-- Just (MkNonEmptyText {unNonEmptyText = " "})
mkNonEmptyText :: (MonadFail m) => T.Text -> m NonEmptyText
mkNonEmptyText x = MkNonEmptyText <$> nonEmptyText x


-- | Type for sanitized 'T.Text' values.
newtype SanitizedText = MkSanitizedText { unSanitizedText :: T.Text } deriving (Eq, Ord, Show)


-- | Safe constructor for 'SanitizedText' values.
--
-- >>> mkSanitizedText "" :: Maybe SanitizedText
-- Nothing
-- >>> mkSanitizedText " " :: Maybe SanitizedText
-- Nothing
-- >>> mkSanitizedText " \r\n\ta \r\n\tb \r\n\t" :: Maybe SanitizedText
-- Just (MkSanitizedText {unSanitizedText = "a b"})
mkSanitizedText :: (MonadFail m) => T.Text -> m SanitizedText
mkSanitizedText x = MkSanitizedText <$> nonEmptySanitizedText x


-- | Returns the non-empty 'T.Text' argument value or fails if the argument is empty.
--
-- >>> nonEmptyText "" :: Maybe T.Text
-- Nothing
-- >>> nonEmptyText " " :: Maybe T.Text
-- Just " "
nonEmptyText :: (MonadFail m) => T.Text -> m T.Text
nonEmptyText "" = fail "Text is empty"
nonEmptyText x  = pure x


-- | Returns the sanitized, non-empty 'T.Text' argument value or fails if the
-- sanitized argument is empty.
--
-- >>> nonEmptySanitizedText "" :: Maybe T.Text
-- Nothing
-- >>> nonEmptySanitizedText " " :: Maybe T.Text
-- Nothing
-- >>> nonEmptySanitizedText " a " :: Maybe T.Text
-- Just "a"
-- >>> nonEmptySanitizedText " a b " :: Maybe T.Text
-- Just "a b"
-- >>> nonEmptySanitizedText " a  b " :: Maybe T.Text
-- Just "a b"
-- >>> nonEmptySanitizedText " \r\n\ta \r\n\tb \r\n\t" :: Maybe T.Text
-- Just "a b"
nonEmptySanitizedText :: (MonadFail m) => T.Text -> m T.Text
nonEmptySanitizedText = nonEmptyText . sanitizeText


-- | Removes leading and trailing whitespaces, replaces consecutive whitespaces
-- with a single space.
--
-- >>> sanitizeText ""
-- ""
-- >>> sanitizeText " "
-- ""
-- >>> sanitizeText "a "
-- "a"
-- >>> sanitizeText " a"
-- "a"
-- >>> sanitizeText " a "
-- "a"
-- >>> sanitizeText " a b "
-- "a b"
-- >>> sanitizeText "  a   b  "
-- "a b"
-- >>> sanitizeText " \r\n\ta \r\n\tb \r\n\t"
-- "a b"
sanitizeText :: T.Text -> T.Text
sanitizeText = withWords id


-- >>> withBS B.tail "hello"
-- "ello"
withBS :: (B.ByteString -> B.ByteString) -> T.Text -> T.Text
withBS = dimap TE.encodeUtf8 TE.decodeUtf8


-- >>> withWords id ""
-- ""
-- >>> withWords id " "
-- ""
-- >>> withWords id " a  b "
-- "a b"
withWords :: ([T.Text] -> [T.Text]) -> T.Text -> T.Text
withWords = dimap T.words T.unwords



-- >>> capitalize ""
-- ""
-- >>> capitalize "a"
-- "A"
-- >>> capitalize "aa"
-- "Aa"
-- >>> capitalize "aa aa"
-- "Aa aa"
-- >>> withWords (fmap capitalize) "aa aa"
-- "Aa Aa"
-- >>> withWords (fmap capitalize) "aa aa" == T.toTitle "aa aa"
-- True
capitalize :: T.Text -> T.Text
capitalize x = T.toUpper (T.take 1 x) <> T.drop 1 x
