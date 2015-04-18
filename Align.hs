module Align (align) where

import Data.List (transpose)
import Colocate (colocate)

--align :: String -> String
align = unlines . colocate . transpose . map words . lines

--main :: IO ()
main = interact align
