--
-- This program will take a list of text files as input and will
-- display them side-by-side to stdout.  NOTE: this will not work
-- properly with files containing tab characters.
--
import Data.List (transpose)
import System.Environment (getArgs)

-- Take a list of lists and find the length of the longest
-- list.  Then pad all rows, if necessary, to that length+1.
-- pad :: a -> [[a]] -> [[a]]
pad x xs = [ ys ++ replicate (max - length ys) x | ys <- xs ]
    where
        max = (maximum . map length) xs + 1

-- Take a list of Strings, each representing the contents of a text
-- file and output a single string corresponding to those text files
-- rendered side-by-side.
-- colocate :: [String] -> String
colocate = unlines . map concat . transpose . map (pad ' ') . pad [] . map lines

-- Get all args and attempt to open and read each one as a text file.
main = (putStrLn . colocate) =<< mapM readFile =<< getArgs
