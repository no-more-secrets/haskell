-- ==============================================================
-- This module contains functionality for wrapping and justifying
-- text, either normal text  or  certain  types of code comments.
-- ==============================================================
module SmartFormat (fmt) where

import Data.List (sort, group)
import Utils     (unfoldrList, merge)

-- Take  number  of  columns  and  input  string  representing  a
-- document and reformats the  text  so  that  it fits within the
-- specified number of columns.
para :: Int -> [String] -> [[String]]
para n = unfoldrList (splitAt =<< longestFit)
  where
    -- Max number of words  that  can  fit  in  width n (possibly
    -- none). By "fit" it  is  meant  to  include the spaces that
    -- would be inserted between words if they were to be joined.
    -- If not even the  first  word  fits  then the function will
    -- return   one   since   the  word  must  go  on   a   line.
    longestFit :: [String] -> Int
    longestFit = max 1             .
                 length            .
                 takeWhile (<=n)   .
                 zipWith (+) [0..] .
                 scanl1 (+)        .
                 map length

-- Generate   an  infinite  sequence  of  indices  which  are  to
-- represent the positions of "slots" between words. The sequence
-- of indices in the returned list  determines the order in which
-- individual space characters are  distributed  among slots when
-- justifying a line.
distribute :: Int -> [Int]
distribute n = cycle $ [0..n]`merge`reverse [0..n]

-- Basically like "unwords" except  it  takes  an  integer and it
-- will ensure (well, most of the  time) that the returned string
-- contains enough spaces between words  so  as  to span a length
-- equal to that  integer.  Exceptions  to  that  are  if  a line
-- contains only a single  word,  or  if  stretching the line for
-- entail adding more spaces than  there are characters; in those
-- exceptional cases the  result  is  equivalent  to just calling
-- "unwords".
justify :: Int -> [String] -> String
justify _ []     = ""
justify _ (x:[]) = x
justify w xs
    | need > noSpace = unwords xs
    | otherwise      = inflate
  where
    noSpace = length (concat xs)
    need    = w - noSpace
    inflate = concat     . merge xs . map (`replicate`' ') .
              map length . group    . sort . take need .
              distribute $ length xs-2

-- Exported drivers
fmt :: Int -> String -> String
fmt n = unlines . map (justify n) . para n . words
