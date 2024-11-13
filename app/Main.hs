module Main (main) where

import Brick (defaultMain)
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Types
  ( AppFunctions (..),
    ErrorMessage (..),
    MpData,
    Postcode (..),
    SuccessMessage (..),
  )
import UserInterface (app, initialApplicationState)
import Web.MembersApi (performMpLookup)

main :: IO ()
main = do
  finalApplicationState <- defaultMain (app appFunctions) initialApplicationState
  putStrLn $ "Final application state: " ++ show finalApplicationState

appFunctions :: AppFunctions
appFunctions =
  AppFunctions
    { readPostcodes = \filePath -> do
        fileExists <- doesFileExist filePath
        if fileExists
          then Right . map Postcode . lines <$> readFile filePath
          else return (Left (MkErrorMessage $ printf "File '%s' does not exist" filePath)),
      lookupMp = performMpLookup,
      writeCsv = \filePath errorMessagesOrData -> case errorMessagesOrData of
        [] -> pure $ Left $ MkErrorMessage "There is nothing to write"
        _ -> do
          let csvContents = foldMap createCsvRow errorMessagesOrData
          Data.ByteString.Lazy.writeFile filePath csvContents
          return $ Right $ MkSuccessMessage (printf "Wrote to %s" filePath)
    }

createCsvRow :: Either ErrorMessage MpData -> ByteString
createCsvRow (Left errorMessage) = CSV.encode [errorMessage]
createCsvRow (Right mpData) = CSV.encode [mpData]
