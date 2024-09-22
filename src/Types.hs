module Types (Postcode (..), Failure (..), ReportData (..), ConstituencyName (..), MemberName (..)) where

newtype Postcode = Postcode {getPostcode :: String}

data Failure = Failure
  { postcode :: Postcode,
    errorMessage :: String
  }

data ReportData = ReportData Postcode ConstituencyName MemberName

newtype ConstituencyName = ConstituencyName {getConstituencyName :: String}

newtype MemberName = MemberName {getMemberName :: String}
