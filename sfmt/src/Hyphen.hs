module Hyphen (hyphenations, dehyphenate) where

import Data.List  ((\\))
import Data.Maybe (fromMaybe)
import Rules      (canHyphenate, exempt)
import Safe       (lastMay, headMay)
import Utils      (keep, splits, endsWith)

-- We can't do this with a  simple  map operation because we need
-- to join adjacent words after removing a hyphen.
dehyphenate :: [String] -> [String]
dehyphenate (x:y:rest)
    | x == "-"       = x:dehyphenate (y:rest)
    | x`endsWith`"-" = dehyphenate $ (init x++y):rest
    | otherwise      = x:dehyphenate (y:rest)
dehyphenate xs       = xs

validSplit :: (String, String) -> Bool
validSplit (xs, ys)
    | null xs || null ys    = True
    | length xs'`elem`[1]   = False
    | length ys'`elem`[1,2] = False
    | otherwise             = fromMaybe err test
      where
        err        = error "something went wrong..."
        test       = canHyphenate <$> lastMay xs <*> headMay ys
        punct      = ".!?,'\":;"
        (xs', ys') = (xs \\ punct, ys \\ punct)

addHyphen :: (String, String) -> (String, String)
addHyphen ("", xs) = ("", xs)
addHyphen (xs, "") = (xs, "")
addHyphen (xs, ys) = (xs++"-", ys)

hyphenations :: String -> [(String, String)]
hyphenations s
    | exempt s  = [(s,[]), ([],s)]
    | otherwise = map addHyphen . keep validSplit . splits $ s
