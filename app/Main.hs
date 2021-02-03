{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Habulara.Dsl    (runIntoHandle)
import           System.Environment   (getArgs)
import           System.Exit          (die)
import           System.IO            (hPutStrLn, stderr, stdout)
import           Text.Printf          (printf)


main :: IO ()
main = do
  [specFile, dataFile] <- getArgs
  specContent <- BL.readFile specFile
  dataContent <- BL.readFile dataFile
  result <- runIntoHandle specContent dataContent stdout
  case result of
    Left err     -> die ("Error while processing records: " <> show err)
    Right (_, n) -> hPutStrLn stderr $ printf "Successfully processed %d record%s. Exiting..." n (if n == 1 then "" else "s" :: String)
