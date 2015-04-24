--------------------------------------------------------------------------------
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

import Data.List (sort, partition)
import Prefix    (commonPrefix, slashes, unslashes)
import Utils     (onLines, groupByKey)

--------------------------------------------------------------------------------
-- Inputs: Max number of targets, file list as output by unzip -l (last column)

optimize :: Int -> String -> String
optimize n = onLines (map unslashes . optimize' . map slashes . sort)
    where optimize' = uncurry (++) . partition (null . last) . distribute n

--------------------------------------------------------------------------------
-- Implementation
-- type Path = [String]
-- distribute :: Int -> [Path] -> [Path]

distribute _ [path] = [path]
distribute n paths
    | (n < length items)  = [foldr1 commonPrefix paths]
    | otherwise           = concatMap recurse (zip3 scaled keys items)
    where
        (keys, items) = (unzip . groupByKey head) paths
        --recurse :: (Int, String, [Path]) -> [Path]
        recurse (m, key, paths') = map (key:) . distribute m . map tail $ paths'
        scaled = [max 1 (n*x `div` sum lengths) | x <- lengths]
            where lengths = map length items
