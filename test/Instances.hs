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
    n <- choose (1, 7 :: Int)
    case n of
      1 -> pure VEmpty
      2 -> VText     <$> arbitrary
      3 -> VInt      <$> arbitrary
      4 -> VDecimal  <$> arbitrary
      5 -> VBoolean  <$> arbitrary
      6 -> VDate     <$> arbitrary
      7 -> VDateTime <$> arbitrary
