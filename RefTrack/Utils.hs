{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module RefTrack.Utils where

import qualified Data.Text as T
import Data.Text (Text)
import RefTrack.Types
import Control.Lens
import Data.Monoid

showPerson :: Person -> Text
showPerson p = p^.forenames<>" "<>p^.surname

showPersonAbbrev :: Person -> Text
showPersonAbbrev p = T.unwords initials<>" "<>p^.surname
  where initials = take 2 $ map (<>".") $ map (T.take 1) $ T.words (p^.forenames)

readPerson :: Text -> Maybe Person
readPerson t
    | [fore,sur] <- splits  = Just $ Person (T.strip fore) (T.strip sur)
    | otherwise             = case T.words t of
                                  []  -> Nothing
                                  [n] -> Just $ Person "" n
                                  ns  -> Just $ Person (T.unwords $ init ns) (last ns)

  where splits = T.splitOn "," t

shortenAuthorList :: Int -> [Person] -> [Person]
shortenAuthorList n ps
    | length ps > n = take (n-1) (init ps) ++ [last ps]
    | otherwise     = ps

conjunctList :: [Text] -> Text
conjunctList ps = T.intercalate ", " (init ps) <> ", and " <> last ps
