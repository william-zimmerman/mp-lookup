module Types (Postcode (..), ErrorMessage (..), ReportData (..), ConstituencyName (..), MemberName (..)) where

newtype Postcode = Postcode {getPostcode :: String}

newtype ErrorMessage = ErrorMessage {getMessage :: String}

data ReportData = ReportData ConstituencyName MemberName

newtype ConstituencyName = ConstituencyName {getConstituencyName :: String}

newtype MemberName = MemberName {getMemberName :: String}
