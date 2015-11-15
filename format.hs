-- This program is kind of like the linux `fmt' command in that
-- it will read in a series of paragraphs and make them fit in
-- the number of columns specified as the first argument.  If no
-- arguments are given then it defaults to a column width of 70.
import Control.Monad ((<=<))
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Safe (headMay, readMay)
import System.Environment (getArgs)
import Utils (byParagraph)

-- Take number of columns and input string and return a
-- transformed input string consisting of the words in
-- the input string broken up into lines that will fit
-- within the width specified by columns.  Note that,
-- when being reconstructed in the output, the words from
-- the input will be separated by exactly one space even
-- if this was not the case in the input and also that
-- paragraphs will be retained and left separated by a
-- single blank line.  If a word is longer than columns
-- then it is put on its own line.
format :: Int -> String -> String
format n = byParagraph (unlines . map unwords . unfoldr oneLine . words)
  where oneLine wrds = if null wrds then Nothing else Just (oneLine' wrds)
        oneLine' = splitAt . max 1 =<< length . takeWhile (<=n) . scanl1 (+) . map ((+1) . length)

-- Read columns from argument list (default to 70) and
-- then start reading from stdin and transform the text.
main = interact . format . fromMaybe 70 . (readMay <=< headMay) =<< getArgs
