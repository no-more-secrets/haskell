--------------------------------------------------------------------------------
-- Align
--
-- This module will take a series of lines and align their columns, where a
-- column is defined as a contiguous set of non-space characters.  The lines
-- may have differing numbers of columns.

module Align (align, main) where

import Colocate (colocate, pad)
import Data.List (transpose)
import Utils (onLines)

align :: [String] -> [String]
align = colocate . transpose . pad [] . map words

main :: IO ()
main = (interact . onLines) align
