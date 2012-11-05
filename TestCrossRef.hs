{-# LANGUAGE OverloadedStrings #-}

import RefTrack.Types
import RefTrack.CrossRef
import Control.Error
import Control.Monad

main = do
  forM_ dois $ \doi->print =<< runEitherT (lookupDoi doi)
  
dois = map DOI ["10.1577/H02-043", "10.1021/jp035514+"]