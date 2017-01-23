-- ──────────────────────────────────────────────────────────────
-- Basic word wrapping with hyphenation
-- ──────────────────────────────────────────────────────────────
module Wrap (wrapPara) where

import Hyphen (hyphenations, dehyphenate)
import Utils  (unfoldrList, remove)

hyphen :: Int -> String -> (String, String)
hyphen n = head . dropWhile tooLong . reverse . hyphenations
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
        (part1, part2)  = hyphen n x
        purge           = remove null

-- Take  number  of columns and input string representing a docu-
-- ment and reformats the text so that it fits within the  speci-
-- fied  number  of  columns.  If a given word is longer than the
-- target  column  size  then it will be put on its own line, and
-- this  line  will of course be longer than the target number of
-- columns.
wrapPara :: Int -> [String] -> [[String]]
wrapPara n = unfoldrList (wrapOne n) . dehyphenate
