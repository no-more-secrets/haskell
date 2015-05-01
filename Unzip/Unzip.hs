--------------------------------------------------------------------------------
-- Unzip Optimizer
--
-- Takes an integer count (n) and a [String] consisting of a list of
-- files and folders in the format output by unzip -l.  It will return
-- a [String] containing a subset of those files/folder (referred to
-- as unzip targets) which will number approximately less or equal
-- to n such that if we selectively unzip only those targets from the
-- zip file then it will recover the entire file structure in the zip.
-- Note that each unzip target in the result, when extracted, will cause
-- a set of files/folders to be extracted and this set will not overlap
-- with that of any of the other unzip targets.  In order to avoid race
-- conditions that can arise when multiple processes attempt to create
-- the same folders during the eventual parallel unzip process we need
-- to pre-create some of the folders before commencing with the parallel
-- unzip.  The minimum set of folders necessary to precreate will be
-- output by this function as lines that end with "/".  All other lines
-- represents a file/folder that can act as the target for an individual
-- unzip process.

module Unzip (optimize) where

import Data.List (sort)
import Prefix    (commonPrefix, slashes, unslashes)
import Utils     (groupByKey, uncurry3)

optimize :: Int -> [String] -> [String]
optimize n = map unslashes . distribute n . map slashes . sort

--------------------------------------------------------------------------------
-- Implementation
-- type Path = [String]

--distribute :: Int -> [Path] -> [Path]
distribute _ [p] = [p]
distribute n paths
    | (length items > n) = [foldr1 commonPrefix paths]
    | otherwise          = (uncurry3 recurse) =<< zip3 keys weights items
    where
        --items :: [[Path]]
        (keys, items) = (unzip . groupByKey head) paths
        --recurse :: String -> Int -> [Path] -> [Path]
        recurse key m = map (key:) . distribute m . map tail
        weights = [x*n`div`sum lengths | x <- lengths]
            where lengths = map length items
