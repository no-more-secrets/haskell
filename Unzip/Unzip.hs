-- Unzip Optimizer
--
-- Takes a number of processes and a String consisting of a list of files
-- and folders in the format output by unzip -l.  It will return a String
-- containing a subset of those files/folder (referred to as unzip targets)
-- which will number less or equal to `processes` such that if we selectively
-- unzip only those targets from the zip file then it will recover the entire
-- file structure in the zip.  Note that each unzip target in the result,
-- when extracted, will cause a set of files/folders to be extracted and
-- this set will not overlap with that of any of the other unzip targets.
-- In order to avoid race conditions that can arise when multiple processes
-- attempt to create the same folders during the eventual parallel unzip
-- process we need to pre-create some of the folders before commencing with
-- the parallel unzip.  The minimum set of folders necessary to precreate
-- will be output by this function at the top of the list; each such line
-- will end with "/", and represents a folder that should be precreated.
-- Each subsequent line represents a file/folder that can act as the target
-- for an individual unzip process.

module Unzip (optimize) where

import Data.List (sort, groupBy, partition)
import Prefix    (commonPrefix, slashes, unslashes)
import Utils     (onLines, equating)

--------------------------------------------------------------------------------
-- Interface
-- Inputs: Max number of targets, file list as output by unzip -l (last column)

optimize :: Int -> String -> String
optimize processes = onLines (map unslashes . optimize' . map slashes . sort)
    where optimize' = uncurry (++) . partition (null . last) . distribute processes

--------------------------------------------------------------------------------
-- Implementation

-- distribute :: Int -> [[String]] -> [[String]]
distribute count paths
    | (null . tail) paths    = paths
    | (count < length items) = return (foldr1 commonPrefix paths)
    | otherwise              = recurse =<< (zip (scale count lengths) items)
    where
        items   = groupBy (equating head) paths
        lengths = map length items

        --scale :: Int -> [Int] -> [Int]
        scale count = map (max 1 . floor . (*fromIntegral count)) . normalize

        --normalize :: (Fractional a) => [Int] -> [a]
        normalize xs = map (/ fromIntegral (sum xs)) (map fromIntegral xs)

        --recurse :: (Int, String, [[String]]) -> [[String]]
        recurse (newCount, newPaths) = (map (prefix:) . distribute newCount . map tail) newPaths
            where prefix = (head . head) newPaths
