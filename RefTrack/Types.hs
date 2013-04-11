{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable, TemplateHaskell, TypeFamilies,
             OverloadedStrings, PatternGuards #-}

module RefTrack.Types where

import           Prelude hiding ((.), id)
import           Control.Category
import           Control.Monad
import           Data.Typeable
import           Data.Char (isSpace)
import           Data.Function (on)

import           Control.Lens hiding (Indexable)
import           Data.Acid
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.Hashable
import           Data.IxSet (IxSet, Indexable, ixSet, ixFun)
import qualified Data.IxSet as IS
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import           Data.Monoid
import           Data.SafeCopy
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Aeson.TH

import           Web.PathPieces

newtype RefId  = RefId Text deriving (Show, Ord, Eq, Hashable, Typeable, Read)
$(deriveSafeCopy 0 'base ''RefId)
$(deriveJSON id ''RefId)

instance PathPiece RefId where
    fromPathPiece t = Just $ RefId t
    toPathPiece (RefId t) = t

newtype Tag = Tag Text deriving (Show, Ord, Eq, Hashable, Typeable)
$(deriveSafeCopy 0 'base ''Tag)
$(deriveJSON id ''Tag)

newtype Year = Year Int deriving (Show, Ord, Eq, Typeable)
$(deriveSafeCopy 0 'base ''Year)
$(deriveJSON id ''Year)

data Person = Person { _forenames :: Text
                     , _surname :: Text
                     }
            deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''Person)
$(makeLenses ''Person)
$(deriveJSON (drop 1) ''Person)

data Publication = JournalIssue { _pubFullTitle :: Text
                                , _pubAbbrevTitle :: Maybe Text
                                , _pubVolume :: Int
                                , _pubIssue :: Text
                                }
                 | Proceedings { _pubFullTitle :: Text
                               }
                 | Book { _pubFullTitle :: Text
                        }
                 | OtherPub {}
                 deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''Publication)
$(makeLenses ''Publication)
$(deriveJSON (drop 4) ''Publication)

newtype ArxivId = ArxivId Text deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''ArxivId)
$(deriveJSON id ''ArxivId)

newtype DOI = DOI Text deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''DOI)
$(deriveJSON id ''DOI)

mkDOI :: Text -> Maybe DOI
mkDOI t =
    (DOI . T.takeWhile (not . isSpace))
    `fmap` listToMaybe (filter (`T.isPrefixOf` "10.") $ T.tails t)

data ExternalRef = ArxivRef ArxivId
                 | DOIRef DOI
                 deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''ExternalRef)
$(deriveJSON id ''ExternalRef)

data Ref = Ref { -- * General
                 _refId :: RefId
               , _refTitle :: Text
               , _refPublication :: Publication
               , _refAuthors :: [Person]
               , _refPubDate :: Maybe Day
               , _refYear :: Year
               , _refExternalRefs :: [ExternalRef]
               , _refTags :: Set Tag
               , _refAbstract :: Maybe Text
               }
         deriving (Show, Eq, Typeable)
$(deriveSafeCopy 0 'base ''Ref)
$(makeLenses ''Ref)
$(deriveJSON id ''Day)
$(deriveJSON (drop 4) ''Ref)
instance Ord Ref where compare = compare `on` view refId

newtype Author = Author Text deriving (Show, Ord, Eq, Typeable)

instance Indexable Ref where
  empty = ixSet [ ixFun $ \ref->[view refId ref]
                , ixFun $ \ref->map (Author . view surname) $ view refAuthors ref
                ]

data FileHash = SHAHash ByteString
              deriving (Show, Ord, Eq, Typeable, Read)
$(deriveSafeCopy 0 'base ''FileHash)

instance PathPiece FileHash where
    fromPathPiece t
        | Just hash <- T.stripPrefix "sha:" t = Just $ SHAHash $ fst $ B16.decode $ TE.encodeUtf8 hash
        | otherwise               = Nothing
    toPathPiece (SHAHash t) = "sha:"<>TE.decodeUtf8 (B16.encode t)

data Document = Document { _docHash :: FileHash
                         , _docTitle :: Maybe Text
                         , _docDateImported :: UTCTime
                         , _docFilename :: FilePath
                         , _docRef :: RefId
                         }
              deriving (Show, Eq, Typeable)
$(deriveSafeCopy 0 'base ''Document)
$(makeLenses ''Document)

instance Ord Document where compare = compare `on` view docHash
instance Indexable Document where
  empty = ixSet [ ixFun $ \doc->[view docRef doc]
                , ixFun $ \doc->[view docHash doc]
                ]

data Repo = Repo { _repoRefs :: IxSet Ref
                 , _repoDocuments :: IxSet Document
                 , _repoTags :: Map Tag (Set RefId)
                 }
            deriving (Show, Eq, Typeable)
$(deriveSafeCopy 0 'base ''Repo)
$(makeLenses ''Repo)

emptyRepo = Repo IS.empty IS.empty M.empty

fillInRefId :: Ref -> Query Repo Ref
fillInRefId ref | RefId "" <- ref^.refId = do
    repo <- view (to id)
    let exists refid = not $ IS.null $ (repo^.repoRefs) IS.@= refid
    return $ set refId (head $ filter (not . exists) $ possibilities repo) ref
    where possibilities :: Repo -> [RefId]
          possibilities repo = map RefId $
              ( let Year year = view refYear ref
                in case ref^.refAuthors of
                       [] -> []
                       firstAuthor:_ ->
                           map (\i->firstAuthor^.surname <> T.pack (show year) <> i)
                           ([""]++map T.singleton ['a'..])
              ) ++
              [ "unknown"<>T.pack (show i) | i <- [1..] ]

fillInRefId ref = return ref

addRef :: Ref -> Update Repo RefId
addRef ref | RefId "" <- ref^.refId = runQuery (fillInRefId ref) >>= addRef
addRef ref = repoRefs %= IS.insert ref >> return (ref^.refId)

delRef :: Ref -> Update Repo ()
delRef ref = repoRefs %= IS.delete ref

getAllRefs :: Query Repo (Set Ref)
getAllRefs = views repoRefs IS.toSet

getRefById :: RefId -> Query Repo (Maybe Ref)
getRefById refId = views repoRefs (IS.getOne . IS.getEQ refId)

getRefDocs :: RefId -> Query Repo (IxSet Document)
getRefDocs refId = views repoDocuments (IS.getEQ refId)

getDocByHash :: FileHash -> Query Repo (Maybe Document)
getDocByHash hash = views repoDocuments (IS.getOne . IS.getEQ hash)

addDocument :: Document -> Update Repo ()
addDocument doc = repoDocuments %= IS.insert doc

makeAcidic ''Repo [ 'addRef, 'delRef, 'fillInRefId, 'getAllRefs
                  , 'getRefById
                  , 'getRefDocs
                  , 'addDocument , 'getDocByHash
                  ]
