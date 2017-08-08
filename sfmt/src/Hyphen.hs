{-# LANGUAGE ViewPatterns #-}

module Hyphen (hyphenate, dehyphenate, isFragment) where

import Data.List (intersperse)
import Utils     (chunks)

import qualified Text.Hyphenation as H

-- The gold standard for determining whether  a word is a hyphen-
-- ated fragment or not. Note that, e.g. "--", although  it  ends
-- in a hyphen, is not considered a hyphenated fragment since the
-- dashes are not intended to signify joining on the  next  word.
isFragment :: String -> Bool
isFragment ""            = False
isFragment "-"           = False
isFragment "--"          = False
isFragment (last -> '-') = True
isFragment _             = False

-- We can't do this with a simple map operation because  we  need
-- to join adjacent words after removing a hyphen.
dehyphenate :: [String] -> [String]
dehyphenate []  = []
dehyphenate [x] = [x]
dehyphenate (x:y:rest)
    | isFragment x = dehyphenate $ (init x++y):rest
    | otherwise    = x:dehyphenate (y:rest)

-- Make hyphenated fragments, but only if the  word  contains  no
-- hyphens, since otherwise the it may not be possible to reverse
-- the hyphenation and get back the same original word.
hyphenate :: String -> [String]
hyphenate s | '-'`elem`s = [s]
            | otherwise  = hyphenate' s
  where
    -- "documentation" ==> ["do-","cu-","ment-","ation"]
    hyphenate' :: String -> [String]
    hyphenate' = map concat . chunks 2 . intersperse "-"
               . H.hyphenate H.english_US
