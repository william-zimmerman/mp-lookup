{-# LANGUAGE DeriveGeneric #-}

module Types (Postcode (..), Failure (..), ReportData (..), Constituency (..), Member (..)) where

import Data.Csv (ToRecord (toRecord), record)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)

newtype Postcode = Postcode {getPostcode :: String}
  deriving (Show)

data Failure = Failure
  { postcode :: Postcode,
    errorMessage :: String
  }
  deriving (Show)

instance ToRecord Failure where
  toRecord (Failure postcode' errorMessage') = record [fromString $ getPostcode postcode', fromString errorMessage']

data ReportData = ReportData Postcode Constituency Member
  deriving (Show, Generic)

instance ToRecord ReportData where
  toRecord (ReportData postcode' constituency' member') =
    record
      [ fromString $ getPostcode postcode',
        fromString $ getConstituencyName constituency',
        fromString $ getMemberName member',
        fromString $ getLatestParty member'
      ]

newtype Constituency = Constituency {getConstituencyName :: String}
  deriving (Show)

data Member = Member
  { getMemberName :: String,
    getLatestParty :: String
  }
  deriving (Show)