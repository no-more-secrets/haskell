import Data.List (sort, sortBy, groupBy, intersperse)
import Data.Function (on)
import Utils (split, join)
import Prefix (commonURLPath)

type Path = [String]

weightsToCounts :: Int -> [Int] -> [Int]

-- always assign one to each item
--processes = 1
--weightsToCounts total xs = replicate (length xs) 1

-- change total so that each file gets one
--processes = length (lines filestxt)
--weightsToCounts total xs = xs

-- calculate floats, then round down, and make all of them at least one
processes = 7
weightsToCounts total xs = map (minOne . floor . (*fromIntegral total) . (/total') . fromIntegral) xs
    where total' = fromIntegral (sum xs)
          minOne n = if n == 0 then 1 else n

--reverse . sortBy (comparing . length) . 

toUnzipList :: Int -> [Path] -> [[Path]]
toUnzipList count paths
    | (itemsCount == 0)     = []
    | (count == 1)          = [paths]
    | (length paths == 1)   = [paths]
    | (itemsCount > count)  = [paths]
    | (itemsCount == count) = items
    | otherwise             = recurse =<< (zip distributions items)
    where
        recurse :: (Int,[Path]) -> [[Path]]
        recurse (c,ps) = (map . map) (theHead++) (toUnzipList c (map tail ps))
            where theHead = [(head . head) ps]

        itemsCount = length items

        items :: [[Path]]
        items = groupBy ((==) `on` head) paths -- paths must be sorted

        distributions :: [Int]
        distributions = weightsToCounts count (map length items)

filesToInstructions :: Int -> String -> String
filesToInstructions p s = unlines . intersperse [] . map commonURLPath . (map . map) (join "/") . toUnzipList p . map (split "/") . sort . lines $ s

noMain :: Int -> String
noMain p = filesToInstructions p filestxt
--main = (putStrLn . filesToInstructions $ filestxt) -- =<< readFile "files.txt"

filestxt = unlines [
    "folderA/",
    "folderA/test.txt",
    "folderA/test3.txt",
    "folderA/folderC/",
    "folderA/folderC/test.txt",
    "folderA/folderC/test3.txt",
    "folderA/folderC/test2.txt",
    "folderA/folderC/folderD/",
    "folderA/folderC/folderD/test.txt",
    "folderA/folderC/folderD/test3.txt",
    "folderA/folderC/folderD/test2.txt",
    "folderA/test2.txt",
    "folderA/folderD/",
    "folderA/folderD/test.txt",
    "folderA/folderD/test3.txt",
    "folderA/folderD/test2.txt",
    "folderA/folderB/",
    "folderA/folderB/test.txt",
    "folderA/folderB/test3.txt",
    "folderA/folderB/folderF/",
    "folderA/folderB/folderF/folderG/",
    "folderA/folderB/folderF/folderG/folderH/",
    "folderA/folderB/folderF/folderG/folderH/test8.txt",
    "folderA/folderB/folderF/folderG/folderH/test.txt",
    "folderA/folderB/folderF/folderG/folderH/test7.txt",
    "folderA/folderB/folderF/folderG/folderH/test3.txt",
    "folderA/folderB/folderF/folderG/folderH/test5.txt",
    "folderA/folderB/folderF/folderG/folderH/test2.txt",
    "folderA/folderB/folderF/folderG/folderH/test4.txt",
    "folderA/folderB/folderF/folderG/folderH/test9.txt",
    "folderA/folderB/folderF/folderG/folderH/test6.txt",
    "folderA/folderB/test2.txt",
    "folderA/folderB/folderD/",
    "folderA/folderB/folderD/test.txt",
    "folderA/folderB/folderD/test3.txt",
    "folderA/folderB/folderD/test2.txt",
    "folderA/folderB/folderE/",
    "folderA/folderB/folderE/test2.txt" ]
