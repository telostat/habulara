module Data.Habulara.Internal.Commons.Time where

import Data.Time (Day, defaultTimeLocale, parseTimeM)


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
