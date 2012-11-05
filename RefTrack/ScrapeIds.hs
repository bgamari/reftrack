{-# LANGUAGE OverloadedStrings #-}

module RefTrack.ScrapeIds ( Extractor
                          , findIds
                          ) where
       
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord)
import RefTrack.Types
import Text.Regex.Posix
import Data.Maybe

type Extractor = String -> Maybe ExternalRef
                 
filterBadness :: String -> String
filterBadness = filter (\c->ord c < 255)

standardDoi :: String -> Maybe ExternalRef
standardDoi = fmap f . listToMaybe . match re
  where re = makeRegexOpts compIgnoreCase defaultExecOpt
             ("10\\.[[:digit:]]\\{4\\}/[[:alnum:]+\\.]\\+" :: String)
        f a = DOIRef $ DOI $ T.pack $ a !! 0
        
pnasDoi :: String -> Maybe ExternalRef
pnasDoi = fmap (\a->DOIRef . DOI $ T.pack $ a !! 1) . listToMaybe . match re
  where re = makeRegexOpts compIgnoreCase defaultExecOpt 
             ("doi/\\(10\\.1073/pnas\\.\\w\\+\\)" :: String)

lenientPnasDoi :: String -> Maybe ExternalRef
lenientPnasDoi = fmap f . listToMaybe . match re
  where re = makeRegexOpts compIgnoreCase defaultExecOpt 
             ("doi10\\.1073pnas\\.\\(\\w\\+\\)" :: String)
        f :: [String] -> ExternalRef
        f a = DOIRef $ DOI $ "10.1073/pnas." `T.append` (T.pack $ a !! 1)
        
oldArxivId = fmap (\a->ArxivRef $ ArxivId $ T.pack $ a !! 1) . listToMaybe . match re
  where re = makeRegex ("arXiv:([[:alpha:]-]+/[[:digit:]]+v[[:digit:]]+)" :: String) :: Regex
        
arxivId = fmap (\a->ArxivRef $ ArxivId $ T.pack $ a !! 1) . listToMaybe . match re
  where re = makeRegex ("arXiv:([[:digit:]]{4}\\.[[:digit:]]{4}v[[:digit:]]+)" :: String) :: Regex
                                                             
extractors :: [Extractor]
extractors = [ standardDoi
             , pnasDoi, lenientPnasDoi
             , oldArxivId, arxivId
             ]

findIds :: String -> [ExternalRef]
findIds c = let c' = filterBadness c
            in mapMaybe (\f->f c') extractors
            
test = do
  mapM_ (\c->print $ findIds $ fst c) tests
  
tests = [ ("www.pnas.org/cgi/doi/10.1073/pnas.1018033108", "") -- PNAS
        , ("www.pnas.org͞cgi͞doi͞10.1073͞pnas.0901178106", "") -- Old PNAS
        , ("10.1146/annurev.biophys.36.040306.132608", "") -- Ann. Rev. Biophys.
        , ("[DOI: 10.1063/1.2219977]", "") -- App. Phys. Lett.
        , ("10.1021/ac000877g", "") -- Anal. Chem
        , ("10.1021/la800329k", "") -- Langmuir
        , ("doi:10.1038/nmeth.1553", "") -- Nature Methods
        , ("doi: 10.1016/j.bpj.2008.11.061", "") -- Biophysical Journal
        , ("DOI 10.1002/bit.23043", "") -- Biotech. and Bioeng.
        , ("arXiv:cond-mat/0303516v1", "") -- Old ArXiv
        , ("arXiv:1102.2934v1", "") -- New ArXiv
        , ("dx.doi.org/10.1021/jp1095344", "") -- J. Phys. Chem. A
        , ("[doi:10.1063/1.3598109]", "")
        , ("doi:10.1166/jnn.2009.2020", "") -- J. Nanosci. and Nanotech.
        ]
