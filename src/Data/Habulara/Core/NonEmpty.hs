{-# LANGUAGE FlexibleInstances #-}

module Data.Habulara.Core.NonEmpty where

import Data.Maybe  (fromMaybe)
import Data.String (IsString(..))


-- | Provides a newtype for forcing non-empty values.
newtype NonEmpty a = MkNonEmpty { unpack :: a } deriving (Eq, Ord, Show)


-- | 'Semigroup' instance for 'NonEmpty' type.
--
-- >>> ("a" :: (NonEmpty String)) <> ("b" :: (NonEmpty String))
-- MkNonEmpty {unpack = "ab"}
instance (Semigroup a) => Semigroup (NonEmpty a) where
  (MkNonEmpty x) <> (MkNonEmpty y) = MkNonEmpty $ x <> y


-- | 'IsString' instance for 'NonEmpty' type.
--
-- This is only useful for testing purposes as we wouldn't get a compile-time
-- error for empty strings.
--
-- >>> "hebele" :: NonEmpty String
-- MkNonEmpty {unpack = "hebele"}
-- >>> import qualified Data.ByteString as B
-- >>> "hubele" :: NonEmpty B.ByteString
-- MkNonEmpty {unpack = "hubele"}
-- >>> import qualified Data.Text as T
-- >>> "zartzurt" :: NonEmpty T.Text
-- MkNonEmpty {unpack = "zartzurt"}
instance (Eq a, Monoid a, IsString a) => IsString (NonEmpty a) where
  fromString = fromMaybe (error "Can not create 'NonEmpty' value from empty string") . nonEmpty . fromString


-- | Smart constructor for 'NonEmpty' values.
--
-- >>> nonEmpty "" :: Maybe (NonEmpty String)
-- Nothing
-- >>> nonEmpty "zartzurt" :: Maybe (NonEmpty String)
-- Just (MkNonEmpty {unpack = "zartzurt"})
nonEmpty :: (Eq a, Monoid a, MonadFail m) => a -> m (NonEmpty a)
nonEmpty x
  | isEmpty x = fail "nonEmpty failure: empty value encountered"
  | otherwise = pure (MkNonEmpty x)


-- | Predicate to check if a given 'Monoid' value is 'mempty' (empty).
--
-- >>> fmap isEmpty (["", " "] :: [String])
-- [True,False]
isEmpty :: (Eq a, Monoid a) => a -> Bool
isEmpty = (==) mempty


-- | Attempts to apply a function to a 'NonEmpty' value.
--
-- The function application may not result in a 'NonEmpty' value, hence the 'MonadFail' constraint.
--
-- >>> nepply (const mempty :: String -> String) (fromString "hello") :: Maybe (NonEmpty String)
-- Nothing
-- >>> nepply (const "" :: String -> String) (fromString "hello") :: Maybe (NonEmpty String)
-- Nothing
-- >>> nepply (flip (<>) " world") (fromString "hello") :: Maybe (NonEmpty String)
-- Just (MkNonEmpty {unpack = "hello world"})
nepply :: (Eq b, Monoid b, MonadFail m) => (a -> b) -> NonEmpty a -> m (NonEmpty b)
nepply f (MkNonEmpty x) = nonEmpty $ f x
