{-# LANGUAGE OverloadedStrings #-}
module RefTrack.CrossRef where

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Data.Functor.Identity
import Network.HTTP
import Network.URI
import RefTrack.Types hiding (Document)
import Text.XML
import Text.XML.Cursor

userName = "bgamari@physics.umass.edu"
passwd = "mudpie11"
hostname = URIAuth { uriUserInfo = ""
                   , uriRegName = "doi.crossref.org"
                   , uriPort = ""
                   }

buildQuery :: [(String,String)] -> String
buildQuery q = "?"++(intercalate "&" $ map (\(k,v)->k++"="++v) q)
               
liftEither :: (Monad m, Error e') => (e -> e') -> Either e a -> ErrorT e' m a
liftEither f (Left e) = throwError $ f e
liftEither f (Right a) = return a

mapErr ::  (Monad m, Error e') => (e -> e') -> ErrorT e m a -> ErrorT e' m a
mapErr f = mapErrorT (>>= g)
  where g (Left e)  = return $ Left $ f e
        g (Right a) = return $ Right a
                 
lookupDoi :: DOI -> ErrorT String IO (Maybe Ref)
lookupDoi (DOI doi) = do
  let uri = URI { uriScheme = "http:"
                , uriAuthority = Just hostname
                , uriPath = "/servlet/query"
                , uriQuery = buildQuery [ ("usr", userName)
                                        , ("pwd", passwd)
                                        , ("format", "xml")
                                        , ("id", normalizeEscape $ T.unpack doi)
                                        ]
                , uriFragment = ""
                }
  resp <- liftEither show =<< (lift $ simpleHTTP $ mkRequest GET uri)
  case resp of
    Response {rspCode=(2,_,_), rspBody=body} -> do
      doc <- liftEither show $ parseLBS def body
      let records = fromDocument doc $/ element "doi_record"
      a <- parseRecordReply $ head $ records
      return $ Just a
    a -> throwError $ show a
       
parseRecordReply :: Monad m => Cursor -> ErrorT String m Ref
parseRecordReply root = do
  let e = root $/ element "crossref" &/ element "error"
  when (not $ null e) $ throwError $ T.unpack $ head $ content $ head e
  
  let j = head $ root $/ element "crossref" &/ element "journal"
      ja = head $ j $/ element "journal_article"
      ji = head $ j $/ element "journal_issue" 
  let issue = JournalIssue { jiVolume = read $ T.unpack $ head
                                        $ ji $/ element "journal_volume" &/ element "volume" &// content
                           , jiFullTitle = T.concat $ ji $/ element "journal_metadata" &/ element "full_title" &/ content
                           , jiAbbrevTitle = listToMaybe $ ji $/ element "journal_metadata" &/ element "abbrev_title" &/ content
                           , jiIssue = T.concat $ ji $/ element "journal_volume" &/ element "issue" &/ content
                           }
  return $ Ref { rId = RefId " "
               , rTitle = T.intercalate " " $ ja $/ element "titles" &// content
               , rIssue = issue
               , rAuthors = []
               , rPubDate = Nothing
               , rType = Article
               , rYear = Year 2010
               , rExternalRefs = []
               }
              
parseDoiRecord :: Cursor -> Ref
parseDoiRecord cursor =  undefined
                                   
main = do
  forM_ dois $ \doi->print =<< runErrorT (lookupDoi doi)
  
dois = map DOI ["10.1577/H02-043", "10.1021/jp035514+"]
