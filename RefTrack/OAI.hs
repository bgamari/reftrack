{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module RefTrack.OAI where

import           Control.Error
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP
import           Network.URI               (URI (..))
import           System.Locale
import           Text.XML
import           Text.XML.Cursor

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

oaiRequest :: URI -> [(String, String)] -> EitherT String IO Document
oaiRequest baseUri args = do
    let uri = baseUri { uriQuery = "?"++urlEncodeVars args }
    resp <- lift $ simpleHTTP $ mkRequest GET uri
    case resp of
        Left e -> left $ show e
        Right body -> bimapEitherT show id $ hoistEither $ parseLBS def $ rspBody body

getRecord :: URI -> String -> Identifier -> EitherT String IO Document
getRecord baseUri metadataPrefix ident = do
    oaiRequest baseUri [ ("verb", "GetRecord")
                       , ("identifier", ident)
                       , ("metadataPrefix", metadataPrefix)
                       ]

parseDate :: String -> Maybe UTCTime
parseDate = parseTime defaultTimeLocale (iso8601DateFormat Nothing)
