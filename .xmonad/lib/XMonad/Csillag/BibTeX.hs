module XMonad.Csillag.BibTeX
    where

import Data.Maybe (isJust, fromJust)
import Text.Parsec.String (parseFromFile)
import Text.BibTeX.Entry (T)
import qualified Text.BibTeX.Parse as BibTeX

import XMonad (X, spawn)
import XMonad.Prompt


bibFiles :: [FilePath]
bibFiles = [ "/home/daniel/library/bibs.bib" ]

ignoreParseErrors x = case x of
                           Left _ -> Nothing
                           Right y -> Just y

getBibs :: IO [T]
getBibs = concatMap fromJust . filter isJust <$> flip mapM bibFiles (\bibfile -> ignoreParseErrors <$> parseFromFile BibTeX.file bibfile)


data BibTeXPrompt = BibTeXPrompt

instance XPrompt BibTeXPrompt where
    showXPrompt BibTeXPrompt = "BibTeX: "

-- bibtexPrompt :: XPConfig -> X ()
-- bibtexPrompt config = mkXPromptWithReturn BibTeXPrompt config compl return >>= maybe (return()) openPaper
--     where compl :: ComplFunction
--           compl = const $ return []
--
--           openPaper :: String -> X ()
--           openPaper searchTerm = spawn $ "xdg-open '" ++ url ++ "'"
