-- This program is kind of like the linux `fmt' command in that
-- it will read in a series of paragraphs and make them fit in
-- the number of columns specified as the first argument.  If no
-- arguments are given then it defaults to a column width of 70.
import Control.Monad ((<=<))
import Data.Maybe (fromMaybe)
import Safe (lastMay, readMay)
import System.Environment (getArgs)
import Utils (byParagraph, unfoldrList)

-- What will be used if no arguments are specified
defaultColumns :: Int
defaultColumns = 70

-- Take number of columns and input string representing a
-- document and reformats the text so that it fits within
-- the specified number of columns while preserving paragraphs.
format :: Int -> String -> String
format n = byParagraph onePara
  where
    -- Takes a single paragraph and reformats it
    onePara :: String -> String
    onePara = unlines . map unwords . unfoldrList oneLine . words
    -- Splits the list of input words such that the list on the
    -- left in the result has total joined length of less than n,
    -- unless the first word itself is too long, in which case
    -- the result will consist of only the first word.
    oneLine :: [String] -> ([String],[String])
    oneLine = splitAt . max 1 =<< longestFit
    -- Max number of words that can fit in width n (possibly none).
    longestFit :: [String] -> Int
    longestFit = length . takeWhile (<=n) . joinedLengths
    -- Integrated lengths (including spaces)
    joinedLengths :: [String] -> [Int]
    joinedLengths = scanl1 (+) . map (+1) . map length

-- Attempt to parse last parameter as integer.  This is so that
-- you can invoke the program with multiple integer arguments
-- which would happen if you were to e.g. create an alias to
-- the program to change the default, i.e.:
--
--   alias format='format 50'
--
-- so that you could invoke it with no arguments but still
-- override the new default by invoking e.g. $ format 80
getColumns :: [String] -> Int
getColumns = fromMaybe defaultColumns . (readMay <=< lastMay)

-- Read columns from argument list (default to 70) and
-- then start reading from stdin and transform the text.
main :: IO ()
main = interact . format . getColumns =<< getArgs
