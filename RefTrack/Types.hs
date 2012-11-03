{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module RefTrack.Types where

import           Control.Category                 
import           Data.Acid
import           Data.Function (on)
import           Data.ByteString (ByteString)
import           Data.Hashable
import           Data.IxSet (IxSet, Indexable, ixSet, ixFun)
import qualified Data.IxSet as IS
import           Control.Lens hiding (Indexable)
import           Data.Map (Map)
import           Data.SafeCopy
import           Data.Set (Set)
import           Data.Text (Text)
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
                        
data Person = Person { _personForenames :: Text
                     , _personSurname :: Text
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
                 deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''Publication)
$(makeLenses ''Publication)

newtype ArxivId = ArxivId Text deriving (Show, Eq)
$(deriveSafeCopy 0 'base ''ArxivId)

newtype DOI = DOI Text deriving (Show, Eq)

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
                , ixFun $ \ref->map (Author . view personSurname) $ view refAuthors ref
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

addRef :: Ref -> Update Repo ()
addRef ref = repoRefs %= IS.insert ref
                  
delRef :: Ref -> Update Repo ()
delRef ref = repoRefs %= IS.delete ref
       
$(makeAcidic ''Repo ['addRef, 'delRef])

