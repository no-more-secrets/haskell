module Unzip (optimize) where

import Data.Function (on)
import Data.List (sort, groupBy, partition)
import Prefix (commonPrefix, slashes, unslashes)
import System.Environment (getArgs)
import Utils (onLines, remove, equating)

--------------------------------------------------------------------------------
-- Interface

-- Takes a number of processes and a String consisting of a list of files
-- and folders in the format output by unzip -l.  It will return a String
-- containing a subset of those files/folder (referred to as unzip targets)
-- which will number less or equal to `processes` such that if we selectively
-- unzip only those targets from the zip file then it will recover the entire
-- file structure in the zip.  Note that each unzip target in the result,
-- when extracted, will case a set of files/folders to be extracted and
-- this set will not overlap with that of any of the other unzip targets.

-- optimize :: Int -> String -> String
optimize processes = onLines (map unslashes . optimize' . map slashes . sort)
    where optimize' = uncurry (++) . partition (null . last) . distribute processes

--------------------------------------------------------------------------------
-- Implementation

-- This is a single file/folder path split on the path separator
type Path = [String]

-- distribute :: Int -> [Path] -> [Path]
distribute count paths
    | (null . tail) paths    = paths
    | (count < length items) = return (foldr1 commonPrefix paths)
    | otherwise              = recurse =<< (zip (scale count lengths) items)
    where
        items   = groupBy (equating head) paths
        lengths = map length items

        --recurse :: (Int, String, [Path]) -> [Path]
        recurse (newCount, newPaths) = (map (prefix:) . distribute newCount . map tail) newPaths
            where prefix = (head . head) newPaths

        --scale :: Int -> [Int] -> [Int]
        scale count = map (max 1 . floor . (*fromIntegral count)) . normalize

--------------------------------------------------------------------------------
-- Utilities

normalize :: (Fractional a) => [Int] -> [a]
normalize xs = map (/ fromIntegral (sum xs)) (map fromIntegral xs)

--------------------------------------------------------------------------------
-- Program to read lines from stdin

main :: IO ()
main = (interact . optimize . read . head) =<< getArgs

--------------------------------------------------------------------------------
-- For testing

mainTest :: IO ()
mainTest = (putStrLn . show . spectrum) =<< readFile "files.txt"
    where spectrum txt = (take 200 . map (length . lines . (`optimize` txt))) [1..]
