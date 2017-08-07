{-# LANGUAGE ViewPatterns #-}

module Hyphen (hyphenate, dehyphenate, isHyp) where

import Data.List (intersperse)
import Utils     (chunks)

import qualified Text.Hyphenation as H

isHyp :: String -> Int
isHyp []           = error "empty isHyp"
isHyp ('-':[])     = 0
isHyp ('-':'-':[]) = 0
isHyp xs | last xs == '-' = 1
         | otherwise      = 0

-- We can't do this with a  simple  map operation because we need
-- to join adjacent words after removing a hyphen.
dehyphenate :: [String] -> [String]
dehyphenate [ ] = [ ]
dehyphenate [x] = [x]
dehyphenate (x:y:rest)
    | isHyp x == 0 = x:dehyphenate (y:rest)
    | otherwise    = dehyphenate $ (init x++y):rest

hyphenate :: String -> [String]
hyphenate s@(elem '-' -> True) = [s]
hyphenate s = hyphens s
  where
    -- "documentation" ==> ["do-","cu-","ment-","ation"]
    hyphens :: String -> [String]
    hyphens = map concat . chunks 2 . intersperse "-" . hyphens'
      where hyphens' = H.hyphenate H.english_US

