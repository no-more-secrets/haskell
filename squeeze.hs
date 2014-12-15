--------------------------------------------------------------------------------
-- Author: David P. Sicilia (December 2014)
--
-- Description:
--
-- This utility will read lines from stdin and will make each line fit within
-- a specified number of characters by possibly removing characters from the
-- middle and replacing with dots.  The maximum line width defaults to 80,
-- but can be specified as an argument to the program.  In fact, to allow
-- customization of the default value the program will read all the command
-- line arguments and take the last one as maximum line width.

import System.Environment (getArgs)
import Data.Maybe         (fromMaybe)
import Safe               (lastMay)
import Utils

--squeezeLine :: Int -> (String -> String)
squeezeLine n s = if length s <= n then s else (snip s ++ dots ++ snip `onReverse` s)
    where snip = take (n`div`2 - 1)
          dots = if even n then ".." else "..."

main = interactLines · squeezeLine · fromMaybe 80 =<< maxLength
    where maxLength = (readIntMay ··· lastMay) `fmap` getArgs
