module Main (main) where

import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import Types (Failure, Postcode (..), ReportData)
import Web.MembersApi (getReportData)

main :: IO ()
main = do
  postcodes <- readPostcodes
  failuresOrReportData <- mapM getReportData postcodes
  let csvContents = foldMap createCsvRow failuresOrReportData
  Data.ByteString.Lazy.writeFile "resources/members.csv" csvContents

readPostcodes :: IO [Postcode]
readPostcodes =
  map Postcode . lines <$> readFile "resources/postcodes.txt"

createCsvRow :: Either Failure ReportData -> ByteString
createCsvRow eitherFailureOrReportData =
  case eitherFailureOrReportData of
    (Left failure) -> CSV.encode [failure]
    (Right reportData) -> CSV.encode [reportData]
