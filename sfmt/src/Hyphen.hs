module Hyphen (hyphenate, dehyphenate) where

import Data.List       (intersperse)
import Data.List.Split (chunksOf)

import qualified Text.Hyphenation as H

-- The gold standard for determining whether  a word is a hyphen-
-- ated fragment or not. Note that, e.g. "--", although  it  ends
-- in a hyphen, is not considered a hyphenated fragment since the
-- dashes are not intended to signify joining on the  next  word.
fragment :: String -> Bool
fragment s | all (=='-') s = False
           | last s == '-' = True && not (fragment (init s))
           | otherwise     = False

-- ["documentation","xyz"] => ["do-","cu-","ment-","ation","xyz"]
hyphenate :: [String] -> [String]
hyphenate = concatMap (\s -> if '-'`elem`s then [s] else word s)
  where word = hyphens . H.hyphenate H.english_US
        hyphens xs = map (++"-") (init xs) ++ [last xs]

-- ["do-","cu-","ment-","ation","xyz"] => ["documentation","xyz"]
dehyphenate :: [String] -> [String]
dehyphenate (x:y:zs) | fragment x = dehyphenate $ (init x++y):zs
                     | otherwise  = x:dehyphenate (y:zs)
dehyphenate xs = xs
