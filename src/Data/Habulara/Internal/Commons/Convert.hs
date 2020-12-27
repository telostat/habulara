{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Habulara.Internal.Commons.Convert where

import           Control.Monad.Except                (MonadError(throwError))
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as BC
import           Data.Habulara.Internal.Commons.Time (parseDateM, parseDateTimeM)
import           Data.Habulara.Types                 (HabularaErrorM)
import           Data.Scientific                     (Scientific, toBoundedInteger)
import qualified Data.Text                           as T
import           Data.Time
                 ( Day(..)
                 , LocalTime(..)
                 , addLocalTime
                 , diffLocalTime
                 , fromGregorian
                 , midnight
                 , nominalDiffTimeToSeconds
                 )


type Converter m a b = (HabularaErrorM m, Show a) => a -> m b


mkConverterFromMaybe :: (a -> Maybe b) -> Converter m a b
mkConverterFromMaybe f x = case f x of
  Nothing -> throwError $ "Can not convert from value. Value was: " <> show x
  Just sv -> pure sv


integerFromScientific :: Converter m Scientific Integer
integerFromScientific x = toInteger <$> mkConverterFromMaybe (toBoundedInteger :: Scientific -> Maybe Int) x


integerFromDay :: Converter m Day Integer
integerFromDay = pure . toModifiedJulianDay


-- >>> integerFromLocalTime $ LocalTime (fromGregorian 1970 1 1) midnight :: Either String Integer
-- Right 0
-- >>> integerFromLocalTime $ LocalTime (fromGregorian 1999 12 31) midnight :: Either String Integer
-- Right 946598400
integerFromLocalTime :: Converter m LocalTime Integer
integerFromLocalTime x = floor <$> rationalFromLocalTime x


epochStart :: LocalTime
epochStart = LocalTime (fromGregorian 1970 1 1) midnight


-- >>> rationalFromLocalTime $ LocalTime (fromGregorian 1970 1 1) midnight :: Either String Rational
-- Right (0 % 1)
-- >>> rationalFromLocalTime $ LocalTime (fromGregorian 1999 12 31) midnight :: Either String Rational
-- Right (946598400 % 1)
rationalFromLocalTime :: Converter m LocalTime Rational
rationalFromLocalTime = pure . toRational . nominalDiffTimeToSeconds . diff
  where
    diff x = x `diffLocalTime` epochStart


dayFromInteger :: Converter m Integer Day
dayFromInteger = pure . ModifiedJulianDay


dateTimeFromRational :: Converter m Rational LocalTime
dateTimeFromRational x = pure $ addLocalTime (fromRational x) epochStart


textFromInteger :: Converter m Integer T.Text
textFromInteger = pure . T.pack . show


textFromDecimal :: Converter m Scientific T.Text
textFromDecimal = pure . T.pack . show


textFromDay :: Converter m Day T.Text
textFromDay = pure . T.pack . show


textFromLocalTime :: Converter m LocalTime T.Text
textFromLocalTime = pure . T.pack . show


bsFromInteger :: Converter m Integer B.ByteString
bsFromInteger = pure . BC.pack . show


bsFromDecimal :: Converter m Scientific B.ByteString
bsFromDecimal = pure . BC.pack . show


bsFromDay :: Converter m Day B.ByteString
bsFromDay = pure . BC.pack . show


parseDateFromString :: String -> Converter m String Day
parseDateFromString fmt = mkConverterFromMaybe (parseDateM fmt)


bsFromLocalTime :: Converter m LocalTime B.ByteString
bsFromLocalTime = pure . BC.pack . show


parseLocalTimeFromString :: String -> Converter m String LocalTime
parseLocalTimeFromString fmt = mkConverterFromMaybe (parseDateTimeM fmt)
