{-# LANGUAGE OverloadedStrings #-}

module RefTrack.CrossRef where

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Control.Applicative
import           Control.Error
import           Data.Maybe
import           Data.List
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Functor.Identity
import           Network.HTTP
import           Network.URI
import           RefTrack.Types hiding (Document)
import           Text.XML
import           Text.XML.Cursor

userName = "bgamari@physics.umass.edu"
passwd = "mudpie11"
hostname = URIAuth { uriUserInfo = ""
                   , uriRegName = "doi.crossref.org"
                   , uriPort = ""
                   }

buildQuery :: [(String,String)] -> String
buildQuery q = "?"++(intercalate "&" $ map (\(k,v)->k++"="++v) q)
               
lookupDoi :: DOI -> EitherT String IO (Maybe Ref)
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
    resp <- (mapEitherT show id . hoistEither) =<< (lift $ simpleHTTP $ mkRequest GET uri)
    case resp of
      Response {rspCode=(2,_,_), rspBody=body} -> do
        doc <- mapEitherT show id $ hoistEither $ parseLBS def body
        let queries = fromDocument doc
                      $/ laxElement "query_result"
                      &/ laxElement "body"
                      &/ laxElement "query"
                      >=> attributeIs "status" "resolved"
        case queries of
          r:_ -> do a <- parseQueryReply r
                    return $ Just a
          []  -> left "No records returned"
      a -> left $ show a
       
ensureNotNull :: (Monad m) => e -> [a] -> EitherT e m a
ensureNotNull e []    = left e
ensureNotNull _ (a:_) = right a

parseQueryReply :: Monad m => Cursor -> EitherT String m Ref
parseQueryReply root = do
    let e = root $/ laxElement "crossref"
                 &/ laxElement "error"
    when (not $ null e) $ left $ T.unpack $ head $ content $ head e
    
    j <- ensureNotNull "Can't find journal element"
         $ root $/ laxElement "crossref"
                &/ laxElement "journal"
    ja <- ensureNotNull "Can't find journal_article element"
          $ j $/ laxElement "journal_article"
    ji <- ensureNotNull "Can't find journal_issue element"
          $ j $/ laxElement "journal_issue" 
    let vol = maybe 0 id $ do jv <- listToMaybe $ ji $/ laxElement "journal_volume"
                                                     &/ laxElement "volume"
                                                     &// content
                              readZ $ T.unpack jv
    let pub = JournalIssue
              { _pubVolume = vol
              , _pubFullTitle = T.concat $ j $/ laxElement "journal_metadata"
                                             &/ laxElement "full_title"
                                             &/ content
              , _pubAbbrevTitle = listToMaybe $ j $/ laxElement "journal_metadata"
                                                  &/ laxElement "abbrev_title"
                                                  &/ content
              , _pubIssue = T.concat $ j $/ laxElement "journal_volume"
                                         &/ laxElement "issue"
                                         &/ content
              }

    let parsePerson e = Person <$> (listToMaybe $ e $/ laxElement "given_name" &/ content)
                               <*> (listToMaybe $ e $/ laxElement "surname" &/ content)
        authors = mapMaybe parsePerson
                  $ ja $/ laxElement "contributors"
                       &/ laxElement "person_name"
                       >=> attributeIs "contributor_role" "author"
    
    let erefs = map (DOIRef . DOI) $ ja $/ laxElement "doi_data"
                                        &/ laxElement "doi"
                                        &/ content
    let pubYear = do year <- listToMaybe $ ja $/ laxElement "publication_date"
                                              &/ laxElement "year"
                                              &/ content
                     readZ $ T.unpack year 
    return $ Ref { _refId = RefId ""
                 , _refTitle = T.strip $ T.intercalate " " $ map (T.unwords . T.words)
                               $ ja $/ laxElement "titles" &// content
                 , _refAuthors = authors
                 , _refPubDate = Nothing
                 , _refYear = Year $ maybe 0 id pubYear
                 , _refExternalRefs = erefs
                 , _refPublication = pub
                 , _refAbstract = Nothing
                 , _refTags = S.empty
                 }
              
