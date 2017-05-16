-- ──────────────────────────────────────────────────────────────
-- Basic word wrapping with hyphenation
-- ──────────────────────────────────────────────────────────────
module Wrap (wrapPara) where

import Data.List (inits, splitAt)

import Hyphen (dehyphenate, hyphenChunks)
import Utils  (unfoldrList)

wrapOne :: Int -> [String] -> ([String], [String])
wrapOne n = (splitAt =<< cut)
  where cut  = max 1 . length . takeWhile fits . tail . inits
        fits = (<=n) . length . unwords . dehyphenate

-- Take  number  of columns and input string representing a docu-
-- ment and reformats the text so that it fits within the  speci-
-- fied  number  of  columns.  If a given word is longer than the
-- target  column  size  then it will be put on its own line, and
-- this  line  will of course be longer than the target number of
-- columns.
wrapPara :: Int -> [String] -> [[String]]
wrapPara n = map dehyphenate    . unfoldrList (wrapOne n)
           . (hyphenChunks =<<) . dehyphenate
