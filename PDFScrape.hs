{-# LANGUAGE TupleSections #-}

import System.Process
import Data.Maybe
import Control.Monad
import System.Environment
import Control.Error
import Data.Acid
import Data.Acid.Remote
import Network
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock
import Control.Applicative

import RefTrack.Types
import RefTrack.ScrapeIds
import RefTrack.CrossRef
import Config

import Data.Digest.Pure.SHA

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

scrape :: FilePath -> IO (Maybe Ref)
scrape f = do
    ids <- findPdfIds f
    print (f,ids)
    refs <- mapM (lookupCrossref crossrefServer) ids
    print refs
    return $ case catMaybes refs of a:[] -> Just a
                                    _    -> Nothing

buildDocument :: RefId -> FilePath -> IO Document
buildDocument refId fname = do
    d <- LBS.readFile fname
    today <- getCurrentTime
    return $ Document { _docHash     = SHAHash $ LBS.toStrict $ bytestringDigest $ sha256 d
                      , _docTitle    = Nothing
                      , _docFilename = fname
                      , _docRef      = refId
                      , _docDateImported = today
                      }

main = do
    args <- getArgs
    refs <- forM args $ \f->fmap (f,) <$> scrape f
    putStrLn $ "Found refs for "++frac (length $ catMaybes refs) (length refs)

    st <- openRemoteState "localhost" (PortNumber 7777)
    forM_ (catMaybes refs) $ \(fname,ref) -> do
        refId <- update st $ AddRef ref
        doc <- buildDocument refId fname
        update st $ AddDocument doc
    closeAcidState st

frac :: Real n => n -> n -> String
frac a b = show (100 * realToFrac a / realToFrac b)++"%"
