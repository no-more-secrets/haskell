-- Author: David P. Sicilia
-- This utility will read lines from stdin and will make each line fit within
-- a specified number of characters by possibly removing characters from the
-- middle and replacing with dots.
-- 2014-12-13

import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Utils

defaultMaxLength = 80

squeezeLine :: Int -> String -> String
squeezeLine maxLength xs
    | (maxLength < minMaxLength) = squeezeLine minMaxLength xs
    | (length xs <= maxLength)   = xs
    | even maxLength             = first ++ ".."  ++ second
    | otherwise                  = first ++ "..." ++ second
        where (first, second) = (cut xs, cut `onReverse` xs)
              cut = take (maxLength `div` 2 - 1) 
              minMaxLength = 4

extractMaxLength :: [String] -> Maybe Int
extractMaxLength = readIntMay ··· (`genIdxMay` 0)

getMaxLengthArg :: IO (Maybe Int)
getMaxLengthArg = extractMaxLength `fmap` getArgs

main = interactLines · squeezeLine · fromMaybe defaultMaxLength =<< getMaxLengthArg
