module Data.Habulara.Internal.Commons.String where

import Data.Char (toLower, toUpper)


-- | Lowers the first letter of the given 'String'.
--
-- >>> lowerFirst ""
-- ""
-- >>> lowerFirst "AAA"
-- "aAA"
lowerFirst :: String -> String
lowerFirst []       = []
lowerFirst (x : xs) = toLower x : xs


-- | Uppers the first letter of the given 'String'.
--
-- >>> upperFirst ""
-- ""
-- >>> upperFirst "aaa"
-- "Aaa"
upperFirst :: String -> String
upperFirst []       = []
upperFirst (x : xs) = toUpper x : xs
