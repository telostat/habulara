module Data.Habulara.Dsl.Operators where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Types as Aeson.Types
import Data.Bifunctor (bimap, first)
import Data.Char (toLower, toUpper)
import Data.Habulara.Core (Value)
import Data.Habulara.Core.Mapping (ValueMapperT)
import qualified Data.Habulara.Core.Operation as O
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)


data Op0
  = AsEmpty
  | AsBool
  | AsDate
  | AsTime
  | AsNumber
  | AsText
  | Not
  | Percentage
  | Trim
  | TrimStart
  | TrimEnd
  | Sanitize
  | Lower
  | Upper
  | Capitalize
  deriving (Eq, Read, Show)


-- >>> Aeson.fromJSON "lower" :: Aeson.Result Op0
-- Success Lower
-- >>> Aeson.fromJSON "alower" :: Aeson.Result Op0
-- Error "Unknown operator: alower"
instance Aeson.FromJSON Op0 where
  parseJSON = Aeson.withText "Op0" $ \v -> maybe (fail $ "Unknown operator: " <> T.unpack v) pure $ readMaybe (upperFirst $ T.unpack v)


data Op1
  = Lookup !T.Text
  | Select !T.Text
  | Peek !T.Text
  | ParseDate !T.Text
  | AddDays !Int
  | ParseTime !T.Text
  | AddSeconds !Scientific
  | Add !Scientific
  | Subtract !Scientific
  | FlipSubtract !Scientific
  | Multiply !Scientific
  | Divide !Scientific
  | FlipDivide !Scientific
  | Append !T.Text
  | Prepend !T.Text
  | Take !Int
  | TakeEnd !Int
  | Drop !Int
  | DropEnd !Int
  | Member ![T.Text]
  | Oneof ![T.Text]
  | Translate ![(T.Text, T.Text)]
  deriving (Show)


instance Aeson.FromJSON Op1 where
  parseJSON = Aeson.withArray "Op1" $ \v -> case V.toList v of
    [Aeson.String "lookup", Aeson.String x] -> pure $ Lookup x
    [Aeson.String "select", Aeson.String x] -> pure $ Select x
    [Aeson.String "peek", Aeson.String x] -> pure $ Peek x
    [Aeson.String "parseDate", Aeson.String x] -> pure $ ParseDate x
    [Aeson.String "addDays", Aeson.Number x] -> pure $ AddDays (floor x)
    [Aeson.String "parseTime", Aeson.String x] -> pure $ ParseTime x
    [Aeson.String "addSeconds", Aeson.Number x] -> pure $ AddSeconds x
    [Aeson.String "add", Aeson.Number x] -> pure $ Add x
    [Aeson.String "subtract", Aeson.Number x] -> pure $ Subtract x
    [Aeson.String "flipSubtract", Aeson.Number x] -> pure $ FlipSubtract x
    [Aeson.String "multiply", Aeson.Number x] -> pure $ Multiply x
    [Aeson.String "divide", Aeson.Number x] -> pure $ Divide x
    [Aeson.String "flipDivide", Aeson.Number x] -> pure $ FlipDivide x
    [Aeson.String "append", Aeson.String x] -> pure $ Append x
    [Aeson.String "prepend", Aeson.String x] -> pure $ Prepend x
    [Aeson.String "take", Aeson.Number x] -> pure $ Take (floor x)
    [Aeson.String "takeEnd", Aeson.Number x] -> pure $ TakeEnd (floor x)
    [Aeson.String "drop", Aeson.Number x] -> pure $ Drop (floor x)
    [Aeson.String "dropEnd", Aeson.Number x] -> pure $ DropEnd (floor x)
    [Aeson.String "member", Aeson.Array x] -> withAesonStringArray Member x
    [Aeson.String "oneof", Aeson.Array x] -> withAesonStringArray Oneof x
    [Aeson.String "translate", Aeson.Object x] -> withAesonStringObject Translate x
    _ -> fail $ "Unknown Op1 spec: " <> show v


withAesonStringArray :: MonadFail m => ([T.Text] -> a) -> V.Vector Aeson.Value -> m a
withAesonStringArray f vs = f <$> ensureStrings vs
  where
    ensureStrings = mapM ensureString . V.toList
    ensureString (Aeson.String v) = pure v
    ensureString x = fail $ "Expected string, received: " <> show x


withAesonStringObject :: MonadFail m => ([(T.Text, T.Text)] -> a) -> Aeson.KeyMap.KeyMap Aeson.Value -> m a
withAesonStringObject f vs = f <$> ensureStrings vs
  where
    ensureStrings = mapM ensureString . fmap (first Aeson.toJSON) . Aeson.KeyMap.toList

    ensureString (Aeson.String k, Aeson.String v) = pure (k, v)
    ensureString (k, x) = fail $ "Expected string, received for: " <> show k <> " value: " <> show x


data Op2 = SplitIx !Int !T.Text deriving (Show)


instance Aeson.FromJSON Op2 where
  parseJSON = Aeson.withArray "Op2" $ \v -> case V.toList v of
    [Aeson.String "splitIx", Aeson.Number x, Aeson.String y] -> pure $ SplitIx (floor x) y
    _ -> fail $ "Unknown Op2 spec: " <> show v


data Op
  = OpN0 Op0
  | OpN1 Op1
  | OpN2 Op2
  deriving (Show)


toOperator :: Op -> (Value -> ValueMapperT)
toOperator (OpN0 AsEmpty) = O.asEmpty
toOperator (OpN0 AsBool) = O.asBool
toOperator (OpN0 AsDate) = O.asDate
toOperator (OpN0 AsTime) = O.asTime
toOperator (OpN0 AsNumber) = O.asNumber
toOperator (OpN0 AsText) = O.asText
toOperator (OpN0 Not) = O.not
toOperator (OpN0 Percentage) = O.percentage
toOperator (OpN0 Trim) = O.trim
toOperator (OpN0 TrimStart) = O.trimStart
toOperator (OpN0 TrimEnd) = O.trimEnd
toOperator (OpN0 Sanitize) = O.sanitize
toOperator (OpN0 Lower) = O.lower
toOperator (OpN0 Upper) = O.upper
toOperator (OpN0 Capitalize) = O.capitalize
toOperator (OpN1 (Lookup x)) = const $ O.lookup (O.text x)
toOperator (OpN1 (Select x)) = const $ O.select (O.text x)
toOperator (OpN1 (Peek x)) = const $ O.peek (O.text x)
toOperator (OpN1 (ParseDate x)) = O.parseDate (O.text x)
toOperator (OpN1 (AddDays x)) = O.parseDate (O.number $ fromIntegral x)
toOperator (OpN1 (ParseTime x)) = O.parseTime (O.text x)
toOperator (OpN1 (AddSeconds x)) = O.parseTime (O.number x)
toOperator (OpN1 (Add x)) = O.add (O.number x)
toOperator (OpN1 (Subtract x)) = O.subtract (O.number x)
toOperator (OpN1 (FlipSubtract x)) = flip O.subtract (O.number x)
toOperator (OpN1 (Multiply x)) = O.multiply (O.number x)
toOperator (OpN1 (Divide x)) = O.divide (O.number x)
toOperator (OpN1 (FlipDivide x)) = flip O.divide (O.number x)
toOperator (OpN1 (Append x)) = O.append (O.text x)
toOperator (OpN1 (Prepend x)) = O.prepend (O.text x)
toOperator (OpN1 (Take x)) = O.take (O.number $ fromIntegral x)
toOperator (OpN1 (TakeEnd x)) = O.takeEnd (O.number $ fromIntegral x)
toOperator (OpN1 (Drop x)) = O.drop (O.number $ fromIntegral x)
toOperator (OpN1 (DropEnd x)) = O.dropEnd (O.number $ fromIntegral x)
toOperator (OpN1 (Member x)) = O.member (S.fromList $ fmap O.text x)
toOperator (OpN1 (Oneof x)) = O.oneof (S.fromList $ fmap O.text x)
toOperator (OpN1 (Translate x)) = O.translate (M.fromList $ fmap (bimap O.text O.text) x)
toOperator (OpN2 (SplitIx x y)) = O.splitIx (O.number $ fromIntegral x) (O.text y)


instance Aeson.FromJSON Op where
  parseJSON v = do
    (name, args) <- parseNameArgs v
    case args of
      [] -> OpN0 <$> (Aeson.parseJSON (Aeson.String name) :: Aeson.Types.Parser Op0)
      [x] -> OpN1 <$> (Aeson.parseJSON (Aeson.Array $ V.fromList [Aeson.String name, x]) :: Aeson.Types.Parser Op1)
      [x, y] -> OpN2 <$> (Aeson.parseJSON (Aeson.Array $ V.fromList [Aeson.String name, x, y]) :: Aeson.Types.Parser Op2)
      _ -> fail $ "Unexpected number of arguments to operator: " <> T.unpack name
    where
      parseNameArgs :: Aeson.Value -> Aeson.Types.Parser (T.Text, [Aeson.Value])
      parseNameArgs = Aeson.withObject "Op" $ \o -> (,) <$> o .: "name" <*> (fromMaybe [] <$> o .:? "args")


compileOperator :: T.Text -> [Op] -> ValueMapperT
compileOperator label [] = O.select (O.text label)
compileOperator _ xs = foldl (\o x -> o >>= toOperator x) (pure O.empty) xs


-- * Helpers


--
-- \$helpers

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x : xs) = toLower x : xs


-- >>> upperFirst "lower"
-- "Lower"
upperFirst :: String -> String
upperFirst [] = []
upperFirst (x : xs) = toUpper x : xs
