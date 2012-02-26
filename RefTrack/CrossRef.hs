type Doi = String

data DoiRecord = DoiRecord { owner :: Doi
                           , crossref :: Crossref
                           } deriving Show
data Journal = Journal { fullTitle :: String
                       , abbrevtitle :: String
                       } deriving Show

data JournalIssue = JournalIssue { volume :: Int
                                 , issue :: String
                                 }

d
