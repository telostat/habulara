module Data.Habulara.Internal.Commons.String where

import Data.Char (toLower)


-- | Lowers the first letter of the given 'String'.
--
-- >>> lowerFirst ""
-- ""
-- >>> lowerFirst "AAA"
-- "aAA"
lowerFirst :: String -> String
lowerFirst []       = []
lowerFirst (x : xs) = toLower x : xs
