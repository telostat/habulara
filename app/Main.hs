module Main where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Habulara.Dsl          (processWithHab)
import           System.Environment         (getArgs)
import           System.Exit                (die)
import           System.IO                  (hPutStrLn, stderr, stdout)


main :: IO ()
main = do
  (habpath, csvpath) <- getProgramArguments =<< getArgs
  hab <- B.readFile habpath
  csv <- BL.readFile csvpath
  case processWithHab hab csv of
    Left err -> hPutStrLn stderr $ "Error while processing the file: " <> err
    Right oc -> BLC.hPutStr stdout oc


getProgramArguments :: [FilePath] -> IO (FilePath, FilePath)
getProgramArguments [habfile, csvfile] = pure (habfile, csvfile)
getProgramArguments _                  = die "Incorrect number of arguments. Exiting..."
