module Main where

import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as BL
import qualified Data.ByteString.Lazy.Char8         as BLC
import           Data.Habulara.Dsl                  (processWithHab, readHab)
import           Data.Habulara.Internal.Dsl.CodeGen (generateData)
import qualified Data.Text                          as T
import           System.Environment                 (getArgs)
import           System.Exit                        (die, exitFailure)
import           System.IO                          (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  eprog <- getProgramArguments =<< getArgs
  case eprog of
    Left habpath -> do
      hab <- B.readFile habpath
      case readHab hab of
        Left err -> hPutStrLn stderr ("Can not read HAB file: " <> err) >> exitFailure
        Right fs -> putStrLn $ T.unpack $ generateData fs
    Right (habpath, csvpath) -> do
      hab <- B.readFile habpath
      csv <- BL.readFile csvpath
      case processWithHab hab csv of
        Left err -> hPutStrLn stderr ("Error while processing the file: " <> err) >> exitFailure
        Right oc -> BLC.hPutStr stdout oc


getProgramArguments :: [FilePath] -> IO (Either FilePath (FilePath, FilePath))
getProgramArguments [habfile]          = pure $ Left habfile
getProgramArguments [habfile, csvfile] = pure $ Right (habfile, csvfile)
getProgramArguments _                  = die "Incorrect number of arguments. Exiting..."
