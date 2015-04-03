import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Safe (maximumMay)
import System.Environment (getArgs)

--pad :: a -> Int -> [a] -> [a]
pad e max xs = xs ++ replicate (max - length xs) e

--addPadding :: a -> [[a]] -> [[a]]
addPadding e xss = map (pad e . fromMaybe 0 . maximumMay . map length $ xss) xss

--zipText :: [[String]] -> [[String]]
zipText = transpose . map (addPadding ' ') . addPadding ""

--colocate :: [String] -> String
colocate = unlines . map concat . zipText . map lines

--main :: IO ()
main = getArgs >>= (sequence . map readFile) >>= (putStrLn . colocate)
