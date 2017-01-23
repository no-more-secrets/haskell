---------------------------------------------------------------------
-- Columns
--
-- This module will take a list of integers and will read lines from
-- stdin and select the columns corresponding to the input integers
-- in the order in which they are specified.  The first column has
-- index 0.
module Columns (getColumns, main) where

import Safe (atDef)
import System.Environment (getArgs)
import Utils (interactLines, onWords)

getColumns :: [Int] -> [String] -> [String]
getColumns cols = flip map cols . atDef "."

main :: IO ()
main = (interactLines . onWords . getColumns . map read) =<< getArgs
