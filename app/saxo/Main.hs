{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Monad        ((>=>))
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (Name)
import           Data.Habulara
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           System.Environment   (getArgs)
import           System.IO            (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  path <- head <$> getArgs
  content <- BL.readFile path
  case readRecords mapper ',' content of
    Left err -> hPutStrLn stderr $ "Error while reading the header: " <> err
    Right rs -> writeRecords stdout (V.fromList fields) rs


mapper :: [FieldMapper]
mapper =
  [ selectAs "Account" "account"
  , selectAs "Account" "mainAccount" ~> splitHead '/'
  , selectAs "Account" "subAccount" ~> splitTail '/'
  , selectAs "Date" "date" ~> parseDate "%Y%m%d"
  , selectAs "AccountCurrency" "accountCcy"
  , selectAs "Balance" "balance" ~> vDecimal
  , selectAs "OpenPositionsFX" "valueFX" ~> vDecimal
  , selectAs "OpenPositionsFXOptions" "valueFXOptions" ~> vDecimal
  , selectAs "OpenPositionsStock" "valueStock" ~> vDecimal
  , selectAs "OpenPositionsCFD" "valueCFD" ~> vDecimal
  , selectAs "OpenPositionsCFDOption" "valueCFDOption" ~> vDecimal
  , selectAs "OpenPositionsFutures" "valueFutures" ~> vDecimal
  , selectAs "OpenPositionsETO" "valueETO" ~> vDecimal
  , selectAs "OpenPositionsMutualFunds" "valueMutualFunds" ~> vDecimal
  , selectAs "OpenPositionsBonds" "valueBonds" ~> vDecimal
  , selectAs "OpenpositionsCash" "valueCash" ~> vDecimal
  , selectAs "OpenPositionsSP" "valueSP" ~> vDecimal
  , selectAs "ActiveAccount" "isActive" ~> vText >=> booleanMapCI "YES" "NO"
  , selectAs "PendingNonMarginPositions" "pendingNonMarginPositions" ~> vDecimal
  , selectAs "TotalEquity" "totalEquity" ~> vDecimal
  , selectAs "AccountFunding" "accountFunding" ~> vDecimal
  , selectAs "ReservedForFutureUse2" "reserverForFutureUse2" ~> constant "0"
  , selectAs "PartnerAccountKey" "partnerAccountKey" ~> constantEmpty
  , selectAs "ValueDateCashBalance" "cashBalance" ~> vDecimal
  , selectAs "MarginForTrading" "marginForTrading" ~> vDecimal
  , selectAs "RiskGroupProfile" "riskGroupProfile" ~> constant "0"
  , selectAs "AccountRiskProfile" "accountRiskProfile" ~> constant "None"
  , selectAs "AccountLevelMargining" "accountLevelMargining" ~> constant "No"
  , selectAs "CounterpartID" "CounterpartID" ~> vInt
  , selectAs "OtherCollateral" "otherCollateral" ~> constant "0"
  , selectAs "NotAvailableAsMarginCollateral" "notAvailableAsMarginCollateral" ~> vDecimal
  , selectAs "OwnerID" "ownerID" ~> vInt
  , selectAs "IsClientAccount" "isClientAccount" ~> vText >=> booleanMapCI "YES" "NO"
  , selectAs "AccountType" "accountType" ~> vText >=> upper >=> oneOfText (S.fromList ["BLOCK TRADING", "COMMISSION", "NORMAL"])
  , selectAs "AccountSubType" "accountSubType" ~> vText >=> upper >=> oneOfText (S.fromList ["ERROR", "NONE"])
  ]


fields :: [Name]
fields =
  [ "account"
  , "mainAccount"
  , "subAccount"
  , "date"
  , "accountCcy"
  , "balance"
  , "valueFX"
  , "valueFXOptions"
  , "valueStock"
  , "valueCFD"
  , "valueCFDOption"
  , "valueFutures"
  , "valueETO"
  , "valueMutualFunds"
  , "valueBonds"
  , "valueCash"
  , "valueSP"
  , "isActive"
  , "pendingNonMarginPositions"
  , "totalEquity"
  , "accountFunding"
  , "reserverForFutureUse2"
  , "partnerAccountKey"
  , "cashBalance"
  , "marginForTrading"
  , "riskGroupProfile"
  , "accountRiskProfile"
  , "accountLevelMargining"
  , "CounterpartID"
  , "otherCollateral"
  , "notAvailableAsMarginCollateral"
  , "ownerID"
  , "isClientAccount"
  , "accountType"
  , "accountSubType"
  ]


toFahrenheit :: ValueOperator
toFahrenheit x = multiply 9 x >>= divideBy 5 >>= add 32
