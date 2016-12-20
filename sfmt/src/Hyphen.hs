module Hyphen (hyphenations, dehyphenate) where

import Data.List        (intersect)
import Text.Hyphenation (hyphenate, english_US)
import Utils            (splits, endsWith, startsWith)

-- We can't do this with a  simple  map operation because we need
-- to join adjacent words after removing a hyphen.
dehyphenate :: [String] -> [String]
dehyphenate (x:y:rest)
    | x`startsWith`"-" = x:dehyphenate (y:rest)
    | x`endsWith`"-"   = dehyphenate $ (init x++y):rest
    | otherwise        = x:dehyphenate (y:rest)
dehyphenate xs         = xs

hyphenations :: String -> [(String, String)]
hyphenations = map addHyphen . candidates . list
  where
    list :: String -> [String]
    list s = if exempt s then [s] else (hyphenate english_US) s

    candidates :: [String] -> [(String, String)]
    candidates ss = [(concat x, concat y) | (x,y) <- splits ss]

    addHyphen (xs@(_:_), ys@(_:_)) = (xs++"-", ys)
    addHyphen p = p

    -- Empty string is not exempt ?! !?
    exempt :: String -> Bool
    exempt s = not . null $ s`intersect`noHyp
      where noHyp = "/\\0123456789()[]{}!@#$%^&*=_><'`"
