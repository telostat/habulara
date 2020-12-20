module Data.Habulara.Internal.ValuePrimitives where

import qualified Data.ByteString                     as B
import           Data.Habulara.Internal.Commons.Text (nonEmptySanitizedText, nonEmptyText)
import           Data.Habulara.Types                 (Value(..))
import           Data.Scientific                     (Scientific)
import qualified Data.Text                           as T
import           Data.Time                           (Day)


empty :: Value
empty = VEmpty


raw :: B.ByteString -> Value
raw x
  | B.null x = VEmpty
  | otherwise = VRaw x


int :: Integer -> Value
int = VInt


text :: T.Text -> Value
text x
  | T.null x = VEmpty
  | otherwise = VText x


trimmedText :: T.Text -> Value
trimmedText x = maybe VEmpty VText (nonEmptyText $ T.strip x)


sanitizedText :: T.Text -> Value
sanitizedText x = maybe VEmpty VText (nonEmptySanitizedText x)


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
