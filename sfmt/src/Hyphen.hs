module Hyphen (hyphenate, dehyphenate, isFragment) where

import Data.List       (intersperse)
import Data.List.Split (chunksOf)

import qualified Text.Hyphenation as H

-- This is the underlying hyphenation algorithm.
algo :: String -> [String]
algo = H.hyphenate H.english_US

-- The gold standard for determining whether  a word is a hyphen-
-- ated fragment or not. Note that, e.g. "--", although  it  ends
-- in a hyphen, is not considered a hyphenated fragment since the
-- dashes are not intended to signify joining on the  next  word.
isFragment :: String -> Bool
isFragment s | all (=='-') s = False
             | last s == '-' = True
             | otherwise     = False

-- ["documentation","xyz"] => ["do-","cu-","ment-","ation","xyz"]
hyphenate:: [String] -> [String]
hyphenate = concatMap (\s -> if '-'`elem`s then [s] else f s)
  where f = (\xs -> map (++"-") (init xs) ++ [last xs]) . algo

-- ["do-","cu-","ment-","ation","xyz"] => ["documentation","xyz"]
dehyphenate :: [String] -> [String]
dehyphenate (x:y:zs) | isFragment x = dehyphenate $ (init x++y):zs
                     | otherwise    = x:dehyphenate (y:zs)
dehyphenate xs = xs
