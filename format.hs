-- This program is kind of like the linux `fmt' command in that
-- it will read in a series of paragraphs and make them fit in
-- the number of columns specified as the first argument.  If no
-- arguments are given then it defaults to a column width of 70.

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.List (unfoldr, partition)
import Data.Maybe (fromMaybe)
import Safe (headMay, readMay)
import System.Environment (getArgs)
import Utils (split, byParagraph, mapPair)

-- partition a list but always include at least one element
-- on the left.  This requires Maybe since input list can
-- be empty.
partition1 :: (a -> Bool) -> [a] -> Maybe ([a],[a])
partition1 _ [] = Nothing
partition1 f (x:xs) = Just (x:left,right)
  where (left,right) = partition f xs

-- Take number of columns and input string and return a
-- transformed input string consisting of the words in
-- the input string broken up into lines that will fit
-- within the width specified by columns.  Note that,
-- when being reconstructed in the output, the words from
-- the input will be separated by exactly one space even
-- if this was not the case in the input.  If a word is
-- longer than columns then it is put on its own line.
format :: Int -> String -> String
format n = unlines . map unwords . unfoldr oneLine . words
  where
    oneLine :: [String] -> Maybe ([String],[String])
    oneLine = fmap (mapPair (map snd)) . partition1 ((<=n) . fst) . lengths
    lengths = zip . scanl1 (+) =<< map ((+1) . length)

-- Read columns from argument list (default to 70) and
-- then start reading from stdin and transform the text.
main :: IO()
main = do
    columns <- fromMaybe 70 . (readMay <=< headMay) <$> getArgs
    interact $ byParagraph $ format columns
