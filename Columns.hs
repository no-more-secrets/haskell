module Columns where

import System.Environment (getArgs)
import Ranges (expandRanges, readRanges)
import Utils (interactLines)

--argsToAction :: [String] -> String -> String
argsToAction args = unwords . select . words
    where
        select xs = map (xs !!) indexes
        indexes   = (expandRanges . readRanges . unwords) args

main = interactLines =<< fmap argsToAction getArgs
