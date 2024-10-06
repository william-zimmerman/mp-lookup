module Main (main) where

import Brick (defaultMain)
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Types (Failure, Postcode (..), ReportData)
import UserInterface (Outcome (..), app, initialApplicationState)
import Web.MembersApi (getReportData)

main :: IO ()
main = do
  finalApplicationState <- defaultMain (app doWork) initialApplicationState
  putStrLn $ "Final application state: " ++ show finalApplicationState

doWork :: FilePath -> IO Outcome
doWork filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Failure (printf "File %s does not exist" $ show filePath)
    else do
      postcodes <- readPostcodes filePath
      failuresOrReportData <- mapM getReportData postcodes
      let csvContents = foldMap createCsvRow failuresOrReportData
      Data.ByteString.Lazy.writeFile "resources/members.csv" csvContents
      return Success

readPostcodes :: FilePath -> IO [Postcode]
readPostcodes filepath =
  map Postcode . lines <$> readFile filepath

createCsvRow :: Either Failure ReportData -> ByteString
createCsvRow eitherFailureOrReportData =
  case eitherFailureOrReportData of
    (Left failure) -> CSV.encode [failure]
    (Right reportData) -> CSV.encode [reportData]
