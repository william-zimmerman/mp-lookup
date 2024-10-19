module Main (main) where

import Brick (defaultMain)
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Types (Failure, Postcode (..), MpData)
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
      return (Success $ map createListItem failuresOrReportData)

readPostcodes :: FilePath -> IO [Postcode]
readPostcodes filepath =
  map Postcode . lines <$> readFile filepath

createCsvRow :: Either Failure MpData -> ByteString
createCsvRow (Left failure) = CSV.encode [failure]
createCsvRow (Right reportData) = CSV.encode [reportData]

createListItem :: Either Failure MpData -> String
createListItem (Left failure) = show failure
createListItem (Right reportData) = show reportData
