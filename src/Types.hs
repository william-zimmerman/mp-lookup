module Types (Postcode (..), Failure (..), ReportData (..), Constituency (..), Member (..)) where

newtype Postcode = Postcode {getPostcode :: String}

data Failure = Failure
  { postcode :: Postcode,
    errorMessage :: String
  }

data ReportData = ReportData Postcode Constituency Member

newtype Constituency = Constituency {getConstituencyName :: String}

newtype Member = Member {getMemberName :: String}
