{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable, TemplateHaskell, TypeFamilies,
             OverloadedStrings, PatternGuards #-}

module RefTrack.Types where

import           Control.Category
import           Data.Acid
import           Data.Function (on)
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Hashable
import           Data.IxSet (IxSet, Indexable, ixSet, ixFun)
import qualified Data.IxSet as IS
import           Control.Lens hiding (Indexable)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.SafeCopy
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (listToMaybe)
import           Control.Monad
import           Data.Char (isSpace)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Typeable
import           Prelude hiding ((.), id)

newtype RefId  = RefId Text deriving (Show, Ord, Eq, Hashable, Typeable)
$(deriveSafeCopy 0 'base ''RefId)

newtype Tag = Tag Text deriving (Show, Ord, Eq, Hashable, Typeable)
$(deriveSafeCopy 0 'base ''Tag)

newtype Year = Year Int deriving (Show, Ord, Eq, Typeable)
$(deriveSafeCopy 0 'base ''Year)

data Person = Person { _forenames :: Text
                     , _surname :: Text
                     }
            deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''Person)
$(makeLenses ''Person)

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

newtype ArxivId = ArxivId Text deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''ArxivId)

newtype DOI = DOI Text deriving (Show, Eq)
mkDOI :: Text -> Maybe DOI
mkDOI t =
    (DOI . T.takeWhile (not . isSpace))
    `fmap` listToMaybe (filter (`T.isPrefixOf` "10.") $ T.tails t)

$(deriveSafeCopy 0 'base ''DOI)
data ExternalRef = ArxivRef ArxivId
                 | DOIRef DOI
                 deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''ExternalRef)

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
instance Ord Ref where compare = compare `on` view refId

newtype Author = Author Text deriving (Show, Ord, Eq, Typeable)

instance Indexable Ref where
  empty = ixSet [ ixFun $ \ref->[view refId ref]
                , ixFun $ \ref->map (Author . view surname) $ view refAuthors ref
                ]

data FileHash = SHAHash ByteString
              deriving (Show, Ord, Eq, Typeable)
$(deriveSafeCopy 0 'base ''FileHash)

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
                in take 1
                   $ map (\firstAuthor->firstAuthor^.surname <> T.pack (show year))
                   $ ref^.refAuthors
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

makeAcidic ''Repo ['addRef, 'delRef, 'fillInRefId, 'getAllRefs
                  ,'getRefById
                  ]
