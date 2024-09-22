module Main (main) where

import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import Types (ConstituencyName (..), Failure (..), MemberName (..), Postcode (..), ReportData (..))
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
    (Left (Failure postcode errorMessage)) -> CSV.encode [(getPostcode postcode, errorMessage)]
    (Right (ReportData postcode constituency member)) -> CSV.encode [(getPostcode postcode, getConstituencyName constituency, getMemberName member)]
