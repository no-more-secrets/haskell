-- This program is kind of like the linux `fmt' command in that
-- it will read in a series of paragraphs and make them fit in
-- the number of columns specified as the first argument.  If no
-- arguments are given then it defaults to a column width of 70.
import Control.Monad ((<=<))
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Safe (headMay, readMay)
import System.Environment (getArgs)
import Utils (byParagraph, unfoldrList)

-- Take number of columns and input string representing a
-- document and reformats the text so that it fits within
-- the specified number of columns while preserving paragraphs.
format :: Int -> String -> String
format n = byParagraph (unlines . map unwords . unfoldrList oneLine . words)
  where
    oneLine :: [String] -> ([String],[String])
    oneLine = splitAt . max 1 =<< length . takeWhile (<=n) . scanl1 (+) . map ((+1) . length)

-- Read columns from argument list (default to 70) and
-- then start reading from stdin and transform the text.
main = interact . format . fromMaybe 70 . (readMay <=< headMay) =<< getArgs
