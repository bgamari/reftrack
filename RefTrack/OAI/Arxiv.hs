{-# LANGUAGE OverloadedStrings #-}

module RefTrack.OAI.Arxiv ( getRecord
                          ) where

import           Control.Applicative
import           Control.Error
import           Data.Char (isSpace)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Set            as S
import           Network.URI
import           Text.XML
import           Text.XML.Cursor

import           RefTrack.OAI hiding (getRecord)
import           RefTrack.Types

getRecord :: URI -> Identifier -> EitherT String IO Ref
getRecord baseUri id =
    oaiRequest baseUri [ ("verb", "GetRecord")
                       , ("identifier", id)
                       , ("metadataPrefix", "arXiv")
                       ]
    >>= parseRecord . fromDocument

parseAuthor :: Cursor -> Maybe Person
parseAuthor cur =
    Person <$> listToMaybe (cur $/ laxElement "forenames" &/ content)
           <*> listToMaybe (cur $/ laxElement "keyname" &/ content)

parseRecord :: Monad m => Cursor -> EitherT String m Ref
parseRecord cur = do
    metad <- tryHead "Couldn't find arXiv metadata"
             $ cur $/ laxElement "GetRecord"
                   &/ laxElement "record"
                   &/ laxElement "metadata"
                   &/ laxElement "arXiv"
    arxivId <- tryHead "Couldn't find arXiv ID"
               $ metad $/ laxElement "id" &/ content
    let authors = mapMaybe parseAuthor $ metad $/ laxElement "authors"
                                               &/ laxElement "author"
    title <- tryHead "Couldn't find title"
             $ metad $/ laxElement "title" &/ content
    let categories = concatMap (T.split isSpace)
                     $ metad $/ laxElement "categories" &/ content
        abstract = T.intercalate " " $ T.lines $ T.intercalate " " $ map T.strip
                   $ metad $/ laxElement "abstract" &/ content
        created = metad $/ laxElement "created" &/ content
    return $ Ref { _refId = RefId arxivId
                 , _refTitle = title
                 , _refAuthors = authors
                 , _refPubDate = Nothing
                 , _refYear = Year 0
                 , _refExternalRefs = [ ArxivRef $ ArxivId arxivId ]
                 , _refPublication = OtherPub
                 , _refAbstract = if T.null abstract then Nothing
                                                     else Just abstract
                 , _refTags = S.fromList $ map Tag $ map ("arxiv:"<>) categories
                 }
