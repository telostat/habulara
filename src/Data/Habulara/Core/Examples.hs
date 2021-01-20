{-# LANGUAGE OverloadedStrings #-}

module Data.Habulara.Core.Examples where

import           Control.Applicative             (Alternative((<|>)))
import           Control.Monad                   ((>=>))
import           Control.Monad.Except            (MonadError(..))
import           Data.Habulara.Core.Mapping      (mapper)
import           Data.Habulara.Core.Operations   (select)
import           Data.Habulara.Core.Types.Class  (OperatorM)
import           Data.Habulara.Core.Types.Error  (HabularaError)
import           Data.Habulara.Core.Types.IO     (OperatorIO, runOperatorIO)
import           Data.Habulara.Core.Types.Record (Record)
import           Data.Habulara.Core.Types.Value  (Value(..))
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T


-- | Provides an example record for demonstration purposes.
exampleRecord :: Record
exampleRecord = HM.fromList [("a", VText "1"), ("b", VInt 2)]


-- | Provides an 'OperatorIO' runner over the 'exampleRecord'.
runExample :: OperatorIO a -> IO (Either HabularaError a, Record)
runExample = runOperatorIO exampleRecord


-- | Convenience function that works with unboxed 'VText' values, throws an
-- error otherwise.
--
-- >>> runExample (select "a" >>= withText (pure . VText))
-- (Right (VText "1"),fromList [])
-- >>> runExample (select "b" >>= withText (pure . VText))
-- (Left "Expected some VText value but encountered: VInt 2",fromList [])
withText :: OperatorM m => (T.Text -> m Value) -> Value -> m Value
withText p (VText x) = p x
withText _ x         = throwError $ "Expected some VText value but encountered: " <> show x


-- | Adds a prefix to a 'VText' value.
--
-- >>> runExample (select "a" >>= prefix "[")
-- (Right (VText "[1"),fromList [])
prefix :: OperatorM m => T.Text -> Value -> m Value
prefix s = withText (pure . VText . (<>) s)


-- | Adds a suffix to a 'VText' value.
--
-- >>> runExample (select "a" >>= suffix "]")
-- (Right (VText "1]"),fromList [])
suffix :: OperatorM m => T.Text -> Value -> m Value
suffix s = withText (pure . VText . flip (<>) s)


-- | Wraps a textual value with given prefix and suffix.
--
-- >>> runExample (select "a" >>= wrap "[" "]")
-- (Right (VText "[1]"),fromList [])
wrap :: OperatorM m => T.Text -> T.Text -> Value -> m Value
wrap p s = prefix p >=> suffix s


-- | A sample mapper.
--
-- >>> fst <$> runExample mapper1
-- Right (fromList [("a",VText "1"),("A]",VText "1]"),("[A",VText "[1"),("b",VInt 2),("[A]2",VText "[1]"),("c",VEmpty),("[A]1",VText "[1]")])
mapper1 :: OperatorM m => m Record
mapper1 = mapper
  [ ("a", select "a")
  , ("b", select "b")
  , ("c", select "c" <|> pure VEmpty)
  , ("[A", select "a" >>= prefix "[" )
  , ("A]", select "a" >>= suffix "]" )
  , ("[A]1", select "a" >>= prefix "[" >>= suffix "]" )
  , ("[A]2", select "a" >>= wrap "[" "]" )
  ]


-- | Another sample mapper.
--
-- >>> runExample mapper2
-- (Left "Can not find input field with name: c",fromList [("A",VText "1"),("B",VInt 2)])
mapper2 :: OperatorM m => m Record
mapper2 = mapper
  [ ("A", select "a")
  , ("B", select "b")
  , ("C", select "c")
  ]
