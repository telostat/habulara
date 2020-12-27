module Data.Habulara.Internal.Commons.Time where

import Data.Time           (Day, defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (LocalTime)


-- | Parses dates.
--
-- >>> parseDateM "%Y%m%d" "20200101"
-- 2020-01-01
-- >>> parseDateM "%Y%m%d" "20200101-" :: Maybe Day
-- Nothing
-- >>> parseDateM "%Y%m%d-" "20200101-"
-- 2020-01-01
parseDateM :: (MonadFail m) => String -> String -> m Day
parseDateM = parseTimeM True defaultTimeLocale


-- | Parses dates.
--
-- >>> parseDateTimeM "%Y%m%d" "20200101"
-- 2020-01-01 00:00:00
-- >>> parseDateTimeM "%Y%m%d" "20200101-" :: Maybe LocalTime
-- Nothing
-- >>> parseDateTimeM "%Y%m%d-" "20200101-"
-- 2020-01-01 00:00:00
parseDateTimeM :: (MonadFail m) => String -> String -> m LocalTime
parseDateTimeM = parseTimeM True defaultTimeLocale
