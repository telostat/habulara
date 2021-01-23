{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Examples where

import           Control.Applicative       (Alternative((<|>)))
import           Control.Monad             ((>=>))
import           Control.Monad.Except      (MonadError(..), MonadIO)
import           Data.Habulara.Core.Class  (HabularaError, HabularaT, runHabularaT)
import           Data.Habulara.Core.Mapper (Operation, mapRecord, raiseOperationError, select)
import           Data.Habulara.Core.Types  (Label, Record, Value(VEmpty, VInt, VText))
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T


-- | Provides an example record for demonstration purposes.
exampleRecord :: Record
exampleRecord = HM.fromList [("a", VText "1"), ("b", VInt 2)]


-- | Initial state.
initialState :: (Integer, Record)
initialState = (1, HM.empty)


-- >>> runExample [] :: IO (Either HabularaError (Record, OperationState))
-- Right (fromList [],(1,fromList []))
runExample :: HabularaT Record (Integer, Record) IO a -> IO (Either HabularaError (a, (Integer, Record)))
runExample = runHabularaT exampleRecord initialState


-- >>> runExample [] :: IO (Either HabularaError (Record, OperationState))
-- Right (fromList [],(1,fromList []))
runExampleMapper :: [(Label, HabularaT Record (Integer, Record) IO Value)] -> IO (Either HabularaError (Record, (Integer, Record)))
runExampleMapper fm = runExample (mapRecord fm)


-- | Convenience function that works with unboxed 'VText' values, throws an
-- error otherwise.
--
-- >>> fmap fst <$> runExample (select "a" >>= withText (pure . VText))
-- Right (VText "1")
-- >>> fmap fst <$> runExample (select "b" >>= withText (pure . VText))
-- Left (HabularaErrorOperation "Expected some VText value but encountered: VInt 2")
withText :: Operation m => (T.Text -> m Value) -> Value -> m Value
withText p (VText x) = p x
withText _ x         = raiseOperationError $ "Expected some VText value but encountered: " <> T.pack (show x)


-- | Adds a prefix to a 'VText' value.
--
-- >>> fmap fst <$> runExample (select "a" >>= prefix "[")
-- Right (VText "[1")
prefix :: Operation m => T.Text -> Value -> m Value
prefix s = withText (pure . VText . (<>) s)


-- | Adds a suffix to a 'VText' value.
--
-- >>> fmap fst <$> runExample (select "a" >>= suffix "]")
-- Right (VText "1]")
suffix :: Operation m => T.Text -> Value -> m Value
suffix s = withText (pure . VText . flip (<>) s)


-- | Wraps a textual value with given prefix and suffix.
--
-- >>> fmap fst <$> runExample (select "a" >>= wrap "[" "]")
-- Right (VText "[1]")
wrap :: Operation m => T.Text -> T.Text -> Value -> m Value
wrap p s = prefix p >=> suffix s


-- | A sample mapper.
--
-- >>> fmap fst <$> runExampleMapper mapper1
-- Right (fromList [("a",VText "1"),("A]",VText "1]"),("[A",VText "[1"),("b",VInt 2),("[A]2",VText "[1]"),("c",VEmpty),("[A]1",VText "[1]")])
mapper1 :: MonadIO m => [(Label, HabularaT Record (Integer, Record) m Value)]
mapper1 =
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
-- >>> fmap fst <$> runExampleMapper mapper2
-- Right (fromList [("A",VText "1"),("B",VInt 2),("C",VEmpty)])
mapper2 :: MonadIO m => [(Label, HabularaT Record (Integer, Record) m Value)]
mapper2 =
  [ ("A", select "a")
  , ("B", select "b")
  , ("C", select "c")
  ]
