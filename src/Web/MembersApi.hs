{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MembersApi (getReportData) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    defaultHttpConfig,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
    (=:),
  )
import qualified Types as T
  ( Constituency (Constituency),
    Failure (..),
    Member (Member),
    Postcode (getPostcode),
    ReportData (..),
  )

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

getReportData :: T.Postcode -> IO (Either T.Failure T.ReportData)
getReportData postcode = unpackResponse postcode <$> makeHttpCall postcode

makeHttpCall :: T.Postcode -> IO Response
makeHttpCall postcode = runReq defaultHttpConfig $ do
  v <- req GET (https "members-api.parliament.uk" /: "api" /: "Location" /: "Constituency" /: "Search") NoReqBody jsonResponse $ "searchText" =: T.getPostcode postcode
  return (responseBody v :: Response)

unpackResponse :: T.Postcode -> Response -> Either T.Failure T.ReportData
unpackResponse postcode response =
  case items response of
    [singleItem] -> Right (T.ReportData postcode (retrieveConstituencyName singleItem) (retrieveMemberName singleItem))
    [] -> Left (T.Failure postcode "No results returned for postcode")
    _ -> Left (T.Failure postcode "More than one result returned for postcode")
  where
    retrieveConstituencyName :: SearchResult -> T.Constituency
    retrieveConstituencyName searchResult = T.Constituency $ name $ value searchResult
    retrieveMemberName :: SearchResult -> T.Member
    retrieveMemberName searchResult = T.Member $ nameListAs $ memberValue $ member $ currentRepresentation $ value searchResult