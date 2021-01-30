{-# LANGUAGE FlexibleInstances #-}

module Instances where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Char                 (chr)
import           Data.Habulara.Core        (NonEmpty(..), Value(..))
import qualified Data.Text                 as T
import           Test.QuickCheck           (Arbitrary(..), choose, listOf1)
import           Test.QuickCheck.Instances ()


---------------
-- NON-EMPTY --
---------------


instance Arbitrary (NonEmpty T.Text) where
  -- TODO: Make sure that we produce Unicode characters, too.
  arbitrary = MkNonEmpty . T.pack <$> listOf1 (chr <$> choose (32, 126))


-------------------------------
-- ARBITRARY INSTANCE: VALUE --
-------------------------------


instance Arbitrary Value where
  arbitrary = do
    n <- choose (1, 6 :: Int)
    case n of
      1 -> pure VEmpty
      2 -> VBool   <$> arbitrary
      3 -> VDate   <$> arbitrary
      4 -> VTime   <$> arbitrary
      5 -> VNumber <$> arbitrary
      6 -> VText   <$> arbitrary
