{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module OAI where

import Text.XML.HXT.Core
import Network.HTTP
import Data.List (intercalate)
import Network.URI (URI(..))
import System.Locale
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar

data ErrorCode = BadArgumentErr
               | BadResumptionTokenErr
               | BadVerbErr
               | CannotDisseminateFormatErr
               | IDDoesNotExistErr
               | NoRecordsMatchErr
               | NoMetadataFormatsErr
               | NoSetHierarchyErr
               deriving (Show, Eq)

data OAIError = OAIError ErrorCode String

type Identifier = String

oaiRequest :: URI -> [(String, String)] -> IO String
oaiRequest baseUri args =
        do res <- simpleHTTP $ defaultGETRequest (baseUri {uriQuery="?"++urlEncodeVars args})
           case res of
                Left _ -> error "Connection error"
                Right x -> return $ (rspBody x :: String)

getRecord :: URI -> Identifier -> IOLA () [XmlTree]
getRecord baseUri id =
        do s <- liftIO $ oaiRequest baseUri [ ("verb", "GetRecord")
                                           , ("identifier", id)
                                           , ("metadataPrefix", "oai_dc")
                                           ]
           readString [] s

parseDate :: String -> Maybe UTCTime
parseDate = parseTime defaultTimeLocale (iso8601DateFormat Nothing)

data DCRecord = DCRecord { dcTitle :: Maybe String
                         , dcCreator :: Maybe String
                         , dcSubject :: Maybe String
                         , dcDescription :: Maybe String
                         , dcPublisher :: Maybe String
                         , dcContributor :: Maybe String
                         , dcDate :: Maybe Day
                         , dcType :: Maybe String
                         , dcIdentifier :: Maybe String
                         , dcSource :: Maybe String
                         , dcLanguage :: Maybe String
                         , dcRelation :: Maybe String
                         , dcCoverage :: Maybe String
                         , dcRights :: Maybe String
                         } deriving (Show, Eq)

parseDCRecord =
        proc x -> do md <- getChildren <<< deep (hasName "metadata") -< x
                     title <- getText <<< getChildren <<< deep (hasName "dc:title") -< md
                     creator <- getText <<< getChildren <<< deep (hasName "dc:creator") -< md
                     returnA -< DCRecord { dcTitle = Just title
                                         , dcCreator = Just creator
                                         }

