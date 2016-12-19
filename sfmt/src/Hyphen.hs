module Hyphen (hyphenations) where

import Data.Maybe (fromMaybe)
import Rules      (canHyphenate, exempt)
import Safe       (lastMay, headMay)
import Utils      (keep, splits)

dehyphenate :: [String] -> [String]
dehyphenate = undefined
--dehyphenate (x:y:rest)
--    | x == "-"       = x:dyhyphenate (y:rest)
--    | x`endsWith`"-" = dehyphenate $ (init x++y):rest
--    | otherwise      = x:dyhyphenate (y:rest)
--dehyphenate xs       = xs

validSplit :: (String, String) -> Bool
validSplit (xs, ys)
    | null xs || null ys   = True
    | length xs`elem`[1,2] = False
    | length ys`elem`[1,2] = False
    | otherwise            = fromMaybe err test
      where
        err = error "something went wrong..."
        test = canHyphenate <$> lastMay xs <*> headMay ys

addHyphen :: (String, String) -> (String, String)
addHyphen ("", xs) = ("", xs)
addHyphen (xs, "") = (xs, "")
addHyphen (xs, ys) = (xs++"-", ys)

hyphenations :: String -> [(String, String)]
hyphenations s
    | exempt s  = [(s,[]), ([],s)]
    | otherwise = map addHyphen . keep validSplit . splits $ s
