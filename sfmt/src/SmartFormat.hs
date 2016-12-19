-- ==============================================================
-- This module contains functionality for wrapping and justifying
-- text, either normal text  or  certain  types of code comments.
-- ==============================================================
module SmartFormat (go) where

import Data.List       (sort, group, find, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe      (fromMaybe)
import Safe            (headDef, headNote)
import Hyphen          (hyphenations)
import Utils           (commonPrefixAll, unfoldrList, merge
                       ,byLine, strip, startsWith, remove)

-- ==============================================================
--                      Configuration stuff
-- ==============================================================
-- This is a commonly used function  signature; it means that the
-- function takes a target  column  width  and a multiline string
-- and returns another multiline string formatted in a particular
-- way.
type FMT = Int -> String -> String

-- These are specially recognized line prefixes that will be
-- preserved at the beginning of the lines during formatting.
comments :: [String]
comments = ["--", "#", "*", "//"]

-- ==============================================================
--           Basic wrapping / justification algorithms
-- ==============================================================
hyphenate :: Int -> String -> (String, String)
hyphenate n = head . dropWhile tooLong . reverse . hyphenations
  where tooLong (x,_) = length x > n

wrapOne :: Int -> [String] -> ([String], [String])
wrapOne n = atLeast1 . worker n
  where
    atLeast1 ([],ws) = ([head ws], tail ws)
    atLeast1 zs      = zs

    -- Recursive worker
    worker :: Int -> [String] -> ([String], [String])
    worker _ []         = ([], [])
    worker n (x:xs)
        | n <= 0        = ([], x:xs)
        | length x <= n = (x:left, right)
        | otherwise     = (purge [part1], purge $ part2:xs)
      where
        (left,  right)  = worker (n-length x-1) xs
        (part1, part2)  = hyphenate n x
        purge           = remove null

-- Take number of columns and  input string representing a docum-
-- ent and reformats the text so  that it fits within the specif-
-- ied number of columns. If a given word is longer than the
-- target column size then it will  be  put  on its own line, and
-- this line will of course be  longer  than the target number of
-- columns.
wrapPara :: Int -> [String] -> [[String]]
wrapPara n = unfoldrList (wrapOne n)

-- Basically like "unwords" except it takes an integer and it
-- will ensure that the  returned  string  contains enough spaces
-- between words so as to  span  a  length equal to that integer.
-- Exceptions to that are if a  line contains only a single word.
justify :: Int -> [String] -> String
justify w xs = concat . merge xs  . map (spaces . length) . group
             . sort   . take need . distribute $ length xs-1
  where
    need = w - length (concat xs)
    spaces :: Int -> String
    spaces n = replicate n ' '
    -- Generate an infinite sequence of indices which are to
    -- represent the positions of "slots" between words. The seq-
    -- uence of indices in the returned list determines the order
    -- in which individual space characters are distributed among
    -- slots when justifying a line.
    distribute :: Int -> [Int]
    distribute n
        | n <= 0    = []
        | otherwise = cycle $ [0..n-1]`merge`reverse [0..n-1]

-- Perform the word wrap and justification; this function is int-
-- ended to be called after any preprocessing functions have e.g.
-- removed spaces or comment prefixes.
fmtPara :: FMT
fmtPara n = unlines . map justify' . wrapPara n . words
  where
    justify' = fixLine . justify n
    -- This function will take a  single  line  and will check to
    -- see if there are an "excessive" number of spaces in it
    -- (due to application of the "justify" function) and, if so,
    -- will reduce it to one space  per  word. This is to prevent
    -- lines from appearing where  the  words  are too spread out
    -- and there are too many  spaces.  The two weighting numbers
    -- used in the length comparison  were chosen since they seem
    -- to produce reasonable-looking output (note that only their
    -- ratio is relevant).
    fixLine :: String -> String
    --fixLine s = if 60*length s >= 100*length s' then s' else s
    --fixLine s = if 94*length s >= 100*length s' then s' else s
    --fixLine s = if 85*length s >= 100*length s' then s' else s
    fixLine s = if 80*length s >= 100*length s' then s' else s
      where s' = (unwords . words) s

-- ==============================================================
--                       Formatting wrappers
-- ==============================================================
-- Function will look at the number of leading spaces on the
-- first line and record it. Then, it will strip all leading
-- spaces from all lines, apply the formatting function with red-
-- uced number of columns, then will  re-attach a fixed number of
-- spaces (the amount found on the first line) to all lines, eff-
-- ectively making them line up.
fmtLeadingSpace :: FMT -> FMT
fmtLeadingSpace f n xs = noSpaces (f (n-length prefix)) xs
  where prefix     = takeWhile (' '==) $ xs
        noSpaces f = byLine (prefix++) . f . byLine strip

-- Will apply the fiven formatting  function,  but first will see
-- if the lines in the text  each begin with a known code-comment
-- prefix; if so, this prefix will  be  stripped off of each line
-- before applying the formatting  function,  and then re-applied
-- after. Also, the target column  number given to the formatting
-- function is decreased by the length of the comment prefix.
fmtCommonPrefix :: FMT -> FMT
fmtCommonPrefix f n s = byLine (strip . (newPrefix++))
                      . f (n-newSize)
                      . byLine (drop size)
                      $ s
  where
    prefix       = (findPrefix . commonPrefixAll . lines) s
    newPrefix    = if null prefix then prefix else (prefix++" ")
    size         = length prefix
    newSize      = length newPrefix
    findPrefix s = fromMaybe "" . find (s`startsWith`) $ comments

-- Apply the given formatting  function  to  each paragraph, then
-- rejoin the paragraphs.
fmtMultiPara :: FMT -> FMT
fmtMultiPara f n = intercalate "\n" . map (f n) . map unlines
                 . splitOn [""]     . lines

-- ==============================================================
--                            Driver
-- ==============================================================
go :: FMT
go = fmtLeadingSpace $ fmtCommonPrefix $ fmtMultiPara $ fmtPara
