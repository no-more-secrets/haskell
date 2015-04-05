-- This program will take a list of text files as input and will
-- display them side-by-side to stdout.  NOTE: this will not work
-- properly with files containing tab characters.

import Data.List (transpose)
import System.Environment (getArgs)

-- Take a list of lists and pad all lists to the length of longest + 1.
-- pad :: a -> [[a]] -> [[a]]
pad e xs = map (take n . (++ repeat e)) xs
    where n = (maximum . map length) xs + 1

-- colocate :: [String] -> String
colocate = unlines . map concat . transpose . map (pad ' ') . pad [] . map lines

-- Read arguments, read files, colocate, then print
main = (putStrLn . colocate) =<< mapM readFile =<< getArgs
