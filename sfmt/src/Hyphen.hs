module Hyphen where

import Data.List  (inits, tails, intersect)
import Data.Maybe (fromMaybe)
import Safe       (lastMay, headMay)
import Utils      (keep)

dehyphenate :: [String] -> [String]
dehyphenate = undefined
--dehyphenate (x:y:rest)
--    | x == "-"       = x:dyhyphenate (y:rest)
--    | x`endsWith`"-" = dehyphenate $ (init x++y):rest
--    | otherwise      = x:dyhyphenate (y:rest)
--dehyphenate xs       = xs

-- Test laziness and move into Utils
splits :: [a] -> [([a], [a])]
splits s = zipWith (,) (inits s) (tails s)

-- Empty string is not exempt
exempt :: String -> Bool
exempt s = not . null $ s`intersect`noHyp
  where noHyp = "/\\0123456789()[]{}!@#$%^&*=_><'`"

validSplit :: (String, String) -> Bool
validSplit (xs, ys)
    | null xs || null ys   = True
    | length xs`elem`[1,2] = False
    | length ys`elem`[1,2] = False
    | otherwise            = fromMaybe err test
      where
        err = error "something went wrong..."
        test = canSplit <$> lastMay xs <*> headMay ys

        canSplit :: Char -> Char -> Bool
        canSplit x y
            | y`elem`punct                   = False
            | (not . isVowel) x && isVowel y = True
            | otherwise                      = False
          where
            isVowel = (`elem`vowels)
            vowels  = "aeiouyAEIOUY"
            punct   = "!.?"

addHyphen :: (String, String) -> (String, String)
addHyphen ("", xs) = ("", xs)
addHyphen (xs, "") = (xs, "")
addHyphen (xs, ys) = (xs++"-", ys)

hyphenations :: String -> [(String, String)]
hyphenations s
    | exempt s  = [(s,[]), ([],s)]
    | otherwise = map addHyphen . keep validSplit . splits $ s
