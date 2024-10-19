module Types (Postcode (..), Failure (..), MpData (..), Constituency (..), Member (..), ErrorMessage (..)) where

import Data.Csv (ToRecord (toRecord), record)
import Data.String (IsString (fromString))

newtype Postcode = Postcode {getPostcode :: String}
  deriving (Show)

data Failure = Failure
  { postcode :: Postcode,
    errorMessage :: String
  }
  deriving (Show)

instance ToRecord Failure where
  toRecord (Failure postcode' errorMessage') = record [fromString $ getPostcode postcode', fromString errorMessage']

data MpData = MpData Postcode Constituency Member
  deriving (Show)

instance ToRecord MpData where
  toRecord (MpData postcode' constituency' member') =
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

newtype ErrorMessage = MkErrorMessage
  { unErrorMessage :: String
  }
  deriving (Show)