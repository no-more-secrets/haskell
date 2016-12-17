-- ==============================================================
-- This module contains functionality for wrapping and justifying
-- text, either normal text  or  certain  types of code comments.
-- ==============================================================
module SmartFormat (go) where

import Data.List  (sort, group, find)
import Data.Maybe (fromMaybe)
import Utils      (commonPrefixAll, unfoldrList, merge, byLine
                  ,strip, startsWith)

-- This is a commonly used function  signature; it means that the
-- function takes a target  column  width  and a multiline string
-- and returns another multiline string formatted in a particular
-- way.
type FMT = Int -> String -> String

comments :: [String]
comments = ["--", "#", "*", "//"]

-- Maximum number of words  that  can  fit  in  width n (possibly
-- none). By "fit" it is meant  to  include the spaces that would
-- be   inserted  between  words  if  they  were  to  be  joined.
longest :: Int -> [String] -> Int
longest n = length     . takeWhile (<=n) . zipWith (+) [0..] .
            scanl1 (+) . map length

-- Take  number  of  columns  and  input  string  representing  a
-- document and reformats the  text  so  that  it fits within the
-- specified number of columns. If  a  given  word is longer than
-- the target column size then it  will  be  put on its own line,
-- and this line will of course  be longer than the target number
-- ofcolumns.
wrapPara :: Int -> [String] -> [[String]]
wrapPara n = unfoldrList (splitAt =<< (max 1 . longest n))

-- Generate   an  infinite  sequence  of  indices  which  are  to
-- represent the positions of "slots" between words. The sequence
-- of indices in the returned list  determines the order in which
-- individual space characters are  distributed  among slots when
-- justifying a line.
distribute :: Int -> [Int]
distribute n | n < 0     = []
             | otherwise = cycle $ [0..n]`merge`reverse [0..n]

-- For  convenience;  just  generate  a  string  with  n  spaces.
spaces :: Int -> String
spaces n = replicate n ' '

-- Basically like "unwords" except  it  takes  an  integer and it
-- will ensure that the  returned  string  contains enough spaces
-- between words so as to  span  a  length equal to that integer.
-- Exceptions to that are if a  line contains only a single word.
justify :: Int -> [String] -> String
justify w xs = concat . merge xs . map (spaces . length) . group
             . sort   . take (w-length (concat xs)) . distribute
             $ length xs-2

-- This function will take a single line and will check to see if
-- there are an  "excessive"  number  of  spaces  in  it  (due to
-- application of the "justify" function) and, if so, will reduce
-- it to one space per word. This is to prevent a situation where
-- e.g. a line only contains two words and the `justify` function
-- puts 60 spaces between them to  make  it fit the target column
-- width. The two weighting numbers used in the length comparison
-- were chosen  since  they  seem  to  produce reasonable-looking
-- output   (note   that   only   their   ratio   is   relevant).
fixLine :: String -> String
fixLine s = if 12*length s >= 20*length s' then s' else s
  where s' = (unwords . words) s

-- Perform the word  wrap  and  justification;  this  function is
-- intended to be called  after  any preprocessing functions have
-- e.g.     removed     spaces      or      comment     prefixes.
fmt :: FMT
fmt n = unlines . map (fixLine . justify n) . wrapPara n . words

-- Function will look at  the  number  of  leading  spaces on the
-- first line and record  it.  Then,  it  will  strip all leading
-- spaces from all  lines,  apply  the  formatting  function with
-- reduced number of columns, then  will re-attach a fixed number
-- of spaces (the amount found on  the  first line) to all lines,
-- effectively making them line up.
fmtLeadingSpace :: FMT -> FMT
fmtLeadingSpace f n xs = noSpaces (f (n-length prefix)) xs
  where prefix     = takeWhile (' '==) $ xs
        noSpaces f = byLine (prefix++) . f . byLine strip

-- Will apply the fiven formatting  function,  but first will see
-- if the lines in the text  each begin with a known code-comment
-- prefix; if so, this prefix will  be  stripped off of each line
-- before applying the formatting  function,  and then re-applied
-- after. Also, the target column  number given to the formatting
-- function is decreased by  the  length  of  the comment prefix.
fmtCommonPrefix :: FMT -> FMT
fmtCommonPrefix f n xs = noPrefix (f (n-size)) xs
  where
    prefix     = (findPrefix . commonPrefixAll . lines) xs
    size       = length prefix
    noPrefix f = byLine (prefix++) . f . byLine (drop size) 
    -- Helper function; it will take  a  single string and see if
    -- it begins with any  prefixes  that  are known to represent
    -- code comments. If so it  will return that prefix otherwise
    -- empty string.
    findPrefix :: String -> String
    findPrefix s = fromMaybe "" . find (s`startsWith`) $ prefixes
      where
        -- For each comment prefix  we  will create an additional
        -- variant that ends in  a  space.  In  the final list of
        -- prefixes   order  matters  since  earlier  items  take
        -- precedence, so therefore if  two  items  have a common
        -- prefix then the shorter  one  should come later, hence
        -- the reverse sort.
        prefixes = (reverse . sort) withSpaces
          where withSpaces = comments ++ map (++" ") comments

-- Exported drivers
go :: FMT
go = fmtLeadingSpace $ fmtCommonPrefix $ fmt
