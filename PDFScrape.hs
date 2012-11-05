import System.Process
import Data.Maybe
import Control.Monad
import System.Environment
       
import RefTrack.Types
import RefTrack.ScrapeIds

findPdfIds :: FilePath -> IO [ExternalRef]
findPdfIds file = do
  (code, contents, err) <- readProcessWithExitCode "pdftotext" [file, "-"] ""
  return $ findIds contents

main = do
  args <- getArgs
  res <- forM args $ \f->do
    ids <- findPdfIds f
    print (f,ids)
    return ids
  let found = length $ catMaybes $ map listToMaybe res
      total = length res
  print (found, total)
  print $ realToFrac found / realToFrac total

