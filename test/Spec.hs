import qualified Data.ByteString    as B
import           Data.Habulara.Core (NonEmpty)
import qualified Data.Text          as T
import           Instances          ()
import           Laws               (prop_monoidLeftIdentity, prop_monoidRightIdentity, prop_semigroupAssoc)
import           Test.QuickCheck    (quickCheck, withMaxSuccess)


------------
-- RUNNER --
------------


main :: IO ()
main = do
  -- Let's warmup! Check semigroup and monoid laws for String::
  quickCheck (withMaxSuccess 1000 (prop_semigroupAssoc :: String -> String -> String -> Bool))
  quickCheck (withMaxSuccess 1000 (prop_monoidLeftIdentity :: String -> Bool))
  quickCheck (withMaxSuccess 1000 (prop_monoidRightIdentity :: String -> Bool))

  -- Laws for NonEmpty:
  quickCheck (prop_semigroupAssoc :: NonEmpty B.ByteString -> NonEmpty B.ByteString -> NonEmpty B.ByteString -> Bool)
  quickCheck (prop_semigroupAssoc :: NonEmpty T.Text -> NonEmpty T.Text -> NonEmpty T.Text -> Bool)
