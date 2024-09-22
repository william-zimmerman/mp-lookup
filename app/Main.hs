{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import GHC.Generics
import Network.HTTP.Req

newtype Postcode = Postcode {getPostcode :: String}

newtype ErrorMessage = ErrorMessage {getMessage :: String}

type ConstituencyName = String

type MemberName = String

data Response = Response
  { items :: [SearchResult],
    totalResults :: Int,
    resultContext :: String
  }
  deriving (Show, Generic)

instance FromJSON Response

data SearchResult = SearchResult
  { value :: Constituency
  }
  deriving (Show, Generic)

instance FromJSON SearchResult

data Constituency = Constituency
  { id :: Int,
    name :: String,
    currentRepresentation :: CurrentRepresentation
  }
  deriving (Show, Generic)

instance FromJSON Constituency

data CurrentRepresentation = CurrentRepresentation
  { member :: Member
  }
  deriving (Show, Generic)

instance FromJSON CurrentRepresentation

data Member = Member
  { memberValue :: MemberValue
  }
  deriving (Show)

instance FromJSON Member where
  parseJSON = withObject "Member" $ \v ->
    Member
      <$> v .: "value"

data MemberValue = MemberValue
  { memberValueId :: Int,
    nameListAs :: String
  }
  deriving (Show)

instance FromJSON MemberValue where
  parseJSON = withObject "MemberValue" $ \v ->
    MemberValue
      <$> v .: "id"
      <*> v .: "nameListAs"

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
    (Right (constituencyName, memberName)) -> CSV.encode [(postcode, constituencyName, memberName)]

makeHttpCall :: Postcode -> IO (Postcode, Response)
makeHttpCall postcode = runReq defaultHttpConfig $ do
  v <- req GET (https "members-api.parliament.uk" /: "api" /: "Location" /: "Constituency" /: "Search") NoReqBody jsonResponse $ "searchText" =: getPostcode postcode
  return (postcode, responseBody v :: Response)

unpackResponse :: Response -> Either ErrorMessage (ConstituencyName, MemberName)
unpackResponse response =
  case items response of
    [singleItem] -> Right (getConstituencyName singleItem, getMemberName singleItem)
    [] -> Left (ErrorMessage "No results returned for postcode")
    _ -> Left (ErrorMessage "More than one result returned for postcode")
  where
    getConstituencyName :: SearchResult -> String
    getConstituencyName searchResult = name $ value searchResult
    getMemberName :: SearchResult -> String
    getMemberName searchResult = nameListAs $ memberValue $ member $ currentRepresentation $ value searchResult