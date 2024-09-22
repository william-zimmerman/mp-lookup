{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MembersApi (Response, SearchResult, Constituency, CurrentRepresentation, Member, MemberValue, makeHttpCall, unpackResponse) where

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
import Types as T
  ( ConstituencyName (ConstituencyName),
    ErrorMessage (ErrorMessage),
    MemberName (MemberName),
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

makeHttpCall :: T.Postcode -> IO (T.Postcode, Response)
makeHttpCall postcode = runReq defaultHttpConfig $ do
  v <- req GET (https "members-api.parliament.uk" /: "api" /: "Location" /: "Constituency" /: "Search") NoReqBody jsonResponse $ "searchText" =: getPostcode postcode
  return (postcode, responseBody v :: Response)

unpackResponse :: Response -> Either T.ErrorMessage T.ReportData
unpackResponse response =
  case items response of
    [singleItem] -> Right (ReportData (retrieveConstituencyName singleItem) (retrieveMemberName singleItem))
    [] -> Left (ErrorMessage "No results returned for postcode")
    _ -> Left (ErrorMessage "More than one result returned for postcode")
  where
    retrieveConstituencyName :: SearchResult -> ConstituencyName
    retrieveConstituencyName searchResult = ConstituencyName $ name $ value searchResult
    retrieveMemberName :: SearchResult -> MemberName
    retrieveMemberName searchResult = MemberName $ nameListAs $ memberValue $ member $ currentRepresentation $ value searchResult