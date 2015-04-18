module Columns where

import Safe (atDef)
import System.Environment (getArgs)
import Utils (interactLines)

--argsToAction :: [String] -> String -> String
argsToAction args = unwords . selectFrom . words
    where
        selectFrom xs = map (atDef "" xs) indexes
        indexes       = map read args

--main :: IO ()
main = interactLines =<< argsToAction `fmap` getArgs
