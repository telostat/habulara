{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Habulara.Internal.Commons.Convert where

import           Control.Monad.Except                (MonadError(throwError))
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as BC
import           Data.Habulara.Internal.Commons.Time (parseDateM)
import           Data.Habulara.Types                 (HabularaErrorM)
import           Data.Scientific                     (Scientific, toBoundedInteger)
import qualified Data.Text                           as T
import           Data.Time                           (Day(ModifiedJulianDay, toModifiedJulianDay))


type Converter m a b = (HabularaErrorM m, Show a) => a -> m b


mkConverterFromMaybe :: (a -> Maybe b) -> Converter m a b
mkConverterFromMaybe f x = case f x of
  Nothing -> throwError $ "Can not convert from value. Value was: " <> show x
  Just sv -> pure sv


integerFromScientific :: Converter m Scientific Integer
integerFromScientific x = toInteger <$> mkConverterFromMaybe (toBoundedInteger :: Scientific -> Maybe Int) x


integerFromDay :: Converter m Day Integer
integerFromDay = pure . toModifiedJulianDay


dayFromInteger :: Converter m Integer Day
dayFromInteger = pure . ModifiedJulianDay


textFromInteger :: Converter m Integer T.Text
textFromInteger = pure . T.pack . show


textFromDecimal :: Converter m Scientific T.Text
textFromDecimal = pure . T.pack . show


textFromDay :: Converter m Day T.Text
textFromDay = pure . T.pack . show


bsFromInteger :: Converter m Integer B.ByteString
bsFromInteger = pure . BC.pack . show


bsFromDecimal :: Converter m Scientific B.ByteString
bsFromDecimal = pure . BC.pack . show


bsFromDay :: Converter m Day B.ByteString
bsFromDay = pure . BC.pack . show


parseDateFromString :: String -> Converter m String Day
parseDateFromString fmt = mkConverterFromMaybe (parseDateM fmt)
