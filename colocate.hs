import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Safe (maximumMay)
import System.Environment (getArgs)
import Utils

--pad :: a -> Int -> [a] -> [a]
pad e max xs = xs ++ replicate (max - length xs) e

--addPadding :: a -> [[a]] -> [[a]]
addPadding e xss = map (pad e · fromMaybe 0 · maximumMay · map length $ xss) xss

--colocate :: [String] -> String
colocate = unlines · map concat · transpose · map (addPadding ' ') · addPadding "" · map lines

main = (putStrLn · colocate) ·· (sequence · map readFile) =<< getArgs
