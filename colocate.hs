import Data.List (transpose)
import System.Environment (getArgs)

--pad :: a -> [[a]] -> [[a]]
pad x xs = [ys ++ replicate (max - length ys) x | ys <- xs]
    where max = (maximum . map length) xs

--colocate :: [String] -> String
colocate = unlines . map concat . transpose . map (pad ' ') . pad [] . map lines

main = (putStrLn . colocate) =<< mapM readFile =<< getArgs
