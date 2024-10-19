module Main (main) where

import Brick (defaultMain)
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Types (ErrorMessage(..), Postcode (..), MpData)
import UserInterface (Outcome (..), app, initialApplicationState)
import Web.MembersApi (performMpLookup)

data AppFunctions = AppFunctions {
  readPostcodes :: FilePath -> IO (Either ErrorMessage [Postcode]),
  lookupMp :: Postcode -> IO (Either ErrorMessage MpData)
}

appFunctions :: AppFunctions
appFunctions = AppFunctions {
  readPostcodes = \filePath -> do
    fileExists <- doesFileExist filePath
    if fileExists
      then Right . map Postcode . lines <$> readFile filePath
      else return (Left (MkErrorMessage "File does not exist")),
  lookupMp = performMpLookup
}

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
      postcodes <- readPostcodes' filePath
      failuresOrReportData <- mapM performMpLookup postcodes
      let csvContents = foldMap createCsvRow failuresOrReportData
      Data.ByteString.Lazy.writeFile "resources/members.csv" csvContents
      return (Success $ map createListItem failuresOrReportData)

readPostcodes' :: FilePath -> IO [Postcode]
readPostcodes' filepath =
  map Postcode . lines <$> readFile filepath

createCsvRow :: Either ErrorMessage MpData -> ByteString
createCsvRow (Left errorMessage) = CSV.encode [errorMessage]
createCsvRow (Right mpData) = CSV.encode [mpData]

createListItem :: Either ErrorMessage MpData -> String
createListItem (Left errorMessage) = show errorMessage
createListItem (Right mpData) = show mpData
