import Data.Function (on)
import Data.List (sort, groupBy)
import Prefix (commonPrefix, slashes, unslashes)
import System.Environment (getArgs)
import Utils (onLines, remove, equating)

type Path = [String]

floorOne n | (n < 1)   = 1
           | otherwise = n

--normalize :: (Fractional a) => [Int] -> [a]
normalize xs = map (/sum xsF) xsF
    where xsF = map fromIntegral xs

-- weightsToCounts :: Int -> [Int] -> [Int]
scale count = map (floorOne . floor . (*fromIntegral count)) . normalize

-- toUnzipList :: Int -> [Path] -> [Path]
toUnzipList count paths
    | (null . tail) paths    = paths
    | (count < length items) = return (foldr1 commonPrefix paths)
    | otherwise              = recurse =<< (zip (scale count lengths) items)
    where
        --recurse :: (Int, String, [Path]) -> [Path]
        recurse (newCount, newPaths) = (map (prefix:) . toUnzipList newCount . map tail) newPaths
            where prefix = (head . head) newPaths

        items   = groupBy (equating head) paths
        lengths = map length items

--filesToInstructions :: Int -> String -> String
filesToInstructions processes = onLines (map unslashes . optimize . map slashes . sort)
    where optimize = remove (null . last) . toUnzipList processes

main = (interact . filesToInstructions . read . head) =<< getArgs

--mainTest = (putStrLn . show . spectrum) =<< readFile "files.txt"
--    where spectrum txt = take 400 . map (length . lines . (`filesToInstructions` txt)) $ [1..]
