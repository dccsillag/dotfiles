module XMonad.Csillag.BibTeX
    where

import Data.Maybe (isJust, fromJust)
import Text.Parsec.String (parseFromFile)
import Text.BibTeX.Entry (T)
import qualified Text.BibTeX.Parse as BibTeX


bibFiles :: [FilePath]
bibFiles = [ "/home/daniel/library/bibs.bib" ]

ignoreParseErrors x = case x of
                           Left _ -> Nothing
                           Right y -> Just y

getBibs :: IO [T]
getBibs = concatMap fromJust . filter isJust <$> flip mapM bibFiles (\bibfile -> ignoreParseErrors <$> parseFromFile BibTeX.file bibfile)
