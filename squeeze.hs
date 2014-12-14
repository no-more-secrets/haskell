--------------------------------------------------------------------------------
-- Author: David P. Sicilia
-- Date:   2014-12-13
-- This utility will read lines from stdin and will make each line fit within
-- a specified number of characters by possibly removing characters from the
-- middle and replacing with dots.

import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import Utils

defaultMaxLength = 80

squeezeLine :: Int -> String -> String
squeezeLine maxLength xs
    | (maxLength < 4)          = squeezeLine 4 xs
    | (length xs <= maxLength) = xs
    | otherwise                = left ++ dots ++ right
        where
            (left, right) = (cut xs, cut `onReverse` xs)
            cut  = take (maxLength `div` 2 - 1 )
            dots = if even maxLength then ".." else "..."

getMaxLengthArg :: IO (Maybe Int)
getMaxLengthArg = fmap (readIntMay ··· headMay) getArgs

main = interactLines · squeezeLine · fromMaybe defaultMaxLength =<< getMaxLengthArg
