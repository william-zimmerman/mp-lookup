module Main (main) where

import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import Types (ConstituencyName (..), ErrorMessage (..), MemberName (..), Postcode (..), ReportData (..))
import Web.MembersApi (Response, makeHttpCall, unpackResponse)

main :: IO ()
main = do
  postcodes <- readPostcodes
  responsesByPostcode <- mapM makeHttpCall postcodes
  let csvContents = foldMap createCsvRow responsesByPostcode
  Data.ByteString.Lazy.writeFile "resources/members.csv" csvContents

readPostcodes :: IO [Postcode]
readPostcodes =
  map Postcode . lines <$> readFile "resources/postcodes.txt"

createCsvRow :: (Postcode, Response) -> ByteString
createCsvRow (Postcode postcode, response) =
  case unpackResponse response of
    (Left (ErrorMessage message)) -> CSV.encode [(postcode, message)]
    (Right (ReportData constituency member)) -> CSV.encode [(postcode, getConstituencyName constituency, getMemberName member)]
