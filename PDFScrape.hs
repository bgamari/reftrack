import System.Process
import Data.Maybe
import Control.Monad
import System.Environment
import Control.Error
import Data.Acid
import Data.Acid.Remote
import Network

import RefTrack.Types
import RefTrack.ScrapeIds
import RefTrack.CrossRef
import Config

findPdfIds :: FilePath -> IO [ExternalRef]
findPdfIds file = do
    (code, contents, err) <- readProcessWithExitCode "pdftotext" [file, "-"] ""
    return $ findIds contents

lookupCrossref :: CrossRefServer -> ExternalRef -> IO (Maybe Ref)
lookupCrossref s (DOIRef doi) = do
    res <- runEitherT $ lookupDoi s doi
    case res of
        Left err -> errLn err >> return Nothing
        Right a  -> return a

lookupCrossref _ _ = return Nothing

scrape :: FilePath -> IO (Maybe ExternalRef, Maybe Ref)
scrape f = do
    ids <- findPdfIds f
    print (f,ids)
    refs <- mapM (lookupCrossref crossrefServer) ids
    print refs
    return ( listToMaybe ids
           , case catMaybes refs of a:[] -> Just a
                                    _    -> Nothing
           )

main = do
    args <- getArgs
    (eids, refs) <- unzip `fmap` forM args scrape
    putStrLn $ "Found IDs for "++frac (length $ catMaybes eids) (length eids)
    putStrLn $ "Found refs for "++frac (length $ catMaybes refs) (length eids)

    st <- openRemoteState "localhost" (PortNumber 7777)
    mapM_ (update st . AddRef) $ catMaybes refs
    closeAcidState st

frac :: Real n => n -> n -> String
frac a b = show (100 * realToFrac a / realToFrac b)++"%"
