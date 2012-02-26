import Control.Monad
import Data.Char (ord)
import System.Environment
import RefTrack.Types
import Text.Regex.Posix
import System.Process
import Data.Maybe

type Extractor = String -> Maybe ExternalRef
                 
filterBadness :: String -> String
filterBadness = filter (\c->ord c < 255)

standardDoi = fmap (\a->DOIRef $ DOI $ a !! 0) . listToMaybe . match re
  where re = makeRegexOpts compIgnoreCase defaultExecOpt
             "10\\.[[:digit:]]\\{4\\}/[[:alnum:]+\\.]\\+"
        
pnasDoi = fmap (\a->DOIRef . DOI $ a !! 1) . listToMaybe . match re
  where re = makeRegexOpts compIgnoreCase defaultExecOpt "doi/\\(10\\.1073/pnas\\.\\w\\+\\)"

lenientPnasDoi = fmap f . listToMaybe . match re
  where re = makeRegexOpts compIgnoreCase defaultExecOpt "doi10\\.1073pnas\\.\\(\\w\\+\\)"
        f a = DOIRef $ DOI $ "10.1073/pnas."++(a !! 1)
        
oldArxivId = fmap (\a->ArxivRef $ ArxivId $ a !! 1) . listToMaybe . match re
  where re = makeRegex "arXiv:([[:alpha:]-]+/[[:digit:]]+v[[:digit:]]+)" :: Regex
        
arxivId = fmap (\a->ArxivRef $ ArxivId $ a !! 1) . listToMaybe . match re
  where re = makeRegex "arXiv:([[:digit:]]{4}\\.[[:digit:]]{4}v[[:digit:]]+)" :: Regex
                                                             
extractors :: [Extractor]
extractors = [ standardDoi
             , pnasDoi, lenientPnasDoi
             , oldArxivId, arxivId
             ]

findIds :: String -> [ExternalRef]
findIds c = let c' = filterBadness c
            in mapMaybe (\f->f c') extractors
            
findPdfIds :: FilePath -> IO [ExternalRef]
findPdfIds file = do
  (code, contents, err) <- readProcessWithExitCode "pdftotext" [file, "-"] ""
  return $ findIds contents

main = do
  test
  args <- getArgs
  res <- forM args $ \f->do
    ids <- findPdfIds f
    print (f,ids)
    return ids
  let found = length $ catMaybes $ map listToMaybe res
      total = length res
  print (found, total)
  print $ realToFrac found / realToFrac total

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
        ]

