{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MembersApi (performMpLookup) where

-- https://members-api.parliament.uk/index.html

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
import Text.Printf (printf)
import qualified Types as T
  ( Constituency (Constituency),
    ErrorMessage (..),
    Member (Member),
    MpData (..),
    Postcode (getPostcode),
  )

data ConstituencyMembersSearchServiceResult = ConstituencyMembersSearchServiceResult
  { items :: [ConstituencyItem],
    totalResults :: Int,
    resultContext :: String
  }
  deriving (Show, Generic)

instance FromJSON ConstituencyMembersSearchServiceResult

data ConstituencyItem = ConstituencyItem
  { value :: Constituency
  }
  deriving (Show, Generic)

instance FromJSON ConstituencyItem

data Constituency = Constituency
  { id :: Int,
    name :: String,
    currentRepresentation :: CurrentRepresentation
  }
  deriving (Show, Generic)

instance FromJSON Constituency

data CurrentRepresentation = CurrentRepresentation
  { member :: MemberItem
  }
  deriving (Show, Generic)

instance FromJSON CurrentRepresentation

data MemberItem = MemberItem
  { memberValue :: Member
  }
  deriving (Show)

instance FromJSON MemberItem where
  parseJSON = withObject "MemberItem" $ \v ->
    MemberItem
      <$> v
      .: "value"

data Member = Member
  { memberValueId :: Int,
    nameListAs :: String,
    latestParty :: Party
  }
  deriving (Show)

instance FromJSON Member where
  parseJSON = withObject "Member" $ \v ->
    Member
      <$> v
      .: "id"
      <*> v
      .: "nameListAs"
      <*> v
      .: "latestParty"

data Party = Party
  { partyName :: String,
    abbreviation :: String
  }
  deriving (Show, Generic)

instance FromJSON Party where
  parseJSON = withObject "Party" $ \v ->
    Party
      <$> v
      .: "name"
      <*> v
      .: "abbreviation"

performMpLookup :: T.Postcode -> IO (Either T.ErrorMessage T.MpData)
performMpLookup postcode = unpackSearchResult postcode <$> callConstituencyMembersSearchService postcode

callConstituencyMembersSearchService ::
  T.Postcode -> IO ConstituencyMembersSearchServiceResult
callConstituencyMembersSearchService
  postcode = runReq defaultHttpConfig $ do
    v <- req GET (https "members-api.parliament.uk" /: "api" /: "Location" /: "Constituency" /: "Search") NoReqBody jsonResponse $ "searchText" =: T.getPostcode postcode
    return (responseBody v :: ConstituencyMembersSearchServiceResult)

unpackSearchResult :: T.Postcode -> ConstituencyMembersSearchServiceResult -> Either T.ErrorMessage T.MpData
unpackSearchResult postcode response =
  case items response of
    [singleItem] -> Right (T.MpData postcode (retrieveConstituency singleItem) (retrieveMember singleItem))
    [] -> Left (T.MkErrorMessage $ printf "No results returned for postcode '%s'" (T.getPostcode postcode))
    _ -> Left (T.MkErrorMessage $ printf "More than one result returned for postcode '%s'" (T.getPostcode postcode))
  where
    retrieveConstituency :: ConstituencyItem -> T.Constituency
    retrieveConstituency searchResult = T.Constituency $ name $ value searchResult
    retrieveMember :: ConstituencyItem -> T.Member
    retrieveMember searchResult =
      let mValue = memberValue $ member $ currentRepresentation $ value searchResult
       in T.Member (nameListAs mValue) (partyName $ latestParty mValue)
