{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module RefTrack.Types where

import Data.Typeable
import Data.Time.Calendar
import Data.Time.Clock
import Data.IxSet
import Data.SafeCopy
import Data.Acid

newtype RefId  = RefId String deriving (Show, Ord, Eq, Typeable)
newtype Year = Year Int deriving (Show, Ord, Eq, Typeable)
newtype Tag = Tag String deriving (Show, Ord, Eq, Typeable)
$(deriveSafeCopy  0 'base ''RefId)
$(deriveSafeCopy  0 'base ''Year)
$(deriveSafeCopy  0 'base ''Tag)
                        
data Person = Person { pForenames :: String
                     , pSurname :: String
                     }
            deriving (Show, Eq)
$(deriveSafeCopy  0 'base ''Person)

data JournalIssue = JournalIssue { jiFullTitle :: String
                                 , jiAbbrevTitle :: Maybe String
                                 , jiVolume :: Int
                                 , jiIssue :: String
                                 }
                  deriving (Show, Eq)
$(deriveSafeCopy  0 'base ''JournalIssue)

data RefType = Article | Book deriving (Show, Eq)
$(deriveSafeCopy  0 'base ''RefType)
                      
newtype ArxivId = ArxivId String deriving (Show, Eq)
$(deriveSafeCopy  0 'base ''ArxivId)
newtype DOI = DOI String deriving (Show, Eq)
$(deriveSafeCopy  0 'base ''DOI)
data ExternalRef = ArxivRef ArxivId
                 | DOIRef DOI
                 deriving (Show, Eq)   
$(deriveSafeCopy  0 'base ''ExternalRef)

data Ref = Ref {
               -- * General
                 rId :: RefId
               , rTitle :: String
               , rIssue :: JournalIssue
               , rAuthors :: [Person]
               , rPubDate :: Day
               , rType :: RefType
               , rYear :: Year
               , rExternalRefs :: [ExternalRef]
               }
         deriving (Show, Eq, Typeable)
$(deriveSafeCopy  0 'base ''Ref)
  
newtype Author = Author String deriving (Show, Ord, Eq, Typeable)
instance Indexable Ref where
  empty = ixSet [ ixFun $ \ref->[rId ref]
                , ixFun $ \ref->map (Author . pSurname) $ rAuthors ref
                ]
  
data Document = Document { dDateImported :: UTCTime
                         , dFilename :: FilePath
                         , dMD5sum :: String
                         , dRef :: Ref
                         }
              deriving (Show, Eq)
$(deriveSafeCopy  0 'base ''Document)
                                    
data Repo = Repo [Document] [Tag]
$(deriveSafeCopy  0 'base ''Repo)

-- $(makeAcidic ''Document ['addRef, 'update])
