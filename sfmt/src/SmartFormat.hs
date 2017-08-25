-- ──────────────────────────────────────────────────────────────
-- This module contains functionality for wrapping and justifying
-- text, either normal text  or  certain  types of code comments.
-- ──────────────────────────────────────────────────────────────
module SmartFormat     ( go
                       , justify
                       , Config(..)
                       ) where

import Data.List       (find, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe      (fromMaybe)
import Justify         (justify)
import Utils           (commonPrefixAll, byLine, strip
                       ,startsWith, groupByKey, endsWith)
import Wrap            (wrap)

-- ──────────────────────────────────────────────────────────────
--                        Config / Types
-- ──────────────────────────────────────────────────────────────
data Config = Config { target   :: Int
                     , comments :: [String]
                     } deriving (Show)

-- This is a commonly used function  signature; it means that the
-- function  takes  a  target column width and a multiline string
-- and returns another multiline string formatted in a particular
-- way.
type FMT = Config -> String -> String

-- ──────────────────────────────────────────────────────────────
--                           Formatter
-- ──────────────────────────────────────────────────────────────
-- Perform the word wrap and  justification; this function is in-
-- tended to be  called  after  any  preprocessing functions have
-- e.g. removed spaces or comment prefixes.
fmtPara :: FMT
fmtPara c = unlines . map (justify n) . wrap n . words
  where n = target c

-- ──────────────────────────────────────────────────────────────
--                      Formatting wrappers
-- ──────────────────────────────────────────────────────────────
-- Function  will  look  at  the  number of leading spaces on the
-- first line and record  it.  Then,  it  will  strip all leading
-- spaces  from all lines, apply the formatting function with re-
-- duced number of columns, then will re-attach a fixed number of
-- spaces  (the amount found on the first line) to all lines, ef-
-- fectively making them line up.
fmtLeadingSpace :: FMT -> FMT
fmtLeadingSpace f c xs = noSpaces (f c') xs
  where prefix     = takeWhile (' '==) $ xs
        noSpaces f = byLine (prefix++) . f . byLine strip
        --c'         = Config (target c-length prefix) (comments c)
        c'         = c{ target = target c-length prefix }

-- Will apply the fiven formatting function, but first  will  see
-- if  the lines in the text each begin with a known code-comment
-- prefix;  if  so, this prefix will be stripped off of each line
-- before applying the formatting  function,  and then re-applied
-- after. Also, the target column number given to the  formatting
-- function is decreased by  the  length  of  the comment prefix.
fmtCommonPrefix :: FMT -> FMT
fmtCommonPrefix f c s = let
    prefix       = fromMaybe "" . flip find (comments c)
                 . startsWith . commonPrefixAll . lines $ s
    size         = length prefix
    prefix'      = if (null prefix || (prefix`endsWith`" ")) then
                       prefix
                   else
                       prefix++" "
    size'        = length prefix'
    c'           = Config (target c-size') (comments c)
 in byLine (strip . (prefix'++)) . f c' . byLine (drop size) $ s

-- Apply  the  given  formatting function to each paragraph, then
-- rejoin the paragraphs.
fmtMultiPara :: FMT -> FMT
fmtMultiPara f c = intercalate "\n" . map (f c) . map unlines
                 . splitOn [""]     . lines

-- Here  we group all the lines by whether or not they start with
-- comment markers. Then, we  only  apply the formatting function
-- to those groups that begin with  comments. This is to facilite
-- e.g.  selecting  all the lines in a code file and then running
-- the formatter but only  to  affect  the  lines that begin with
-- comments.
fmtCommentsOnly :: FMT -> FMT
fmtCommentsOnly f c = concat . map process
                    . groupByKey commentStart . lines
  where
    process :: (Maybe String, [String]) -> String
    process (Nothing, ls) =     (unlines ls)
    process (_, ls)       = f c (unlines ls)

    commentStart :: String -> Maybe String
    commentStart line = find (strip line`startsWith`) $ comments c

-- ──────────────────────────────────────────────────────────────
--                            Driver
-- ──────────────────────────────────────────────────────────────
-- We need two fmtMultiPara's  because  we  may of comments split
-- into paragraphs both outside  of  the  common prefix or inside
-- it.
standard = [fmtMultiPara
           --,fmtMultiPara
           ,fmtLeadingSpace
           ,fmtCommonPrefix
           ,fmtMultiPara]

commentsOnly = fmtCommentsOnly:standard

go :: Bool -> FMT
go True  = foldr ($) fmtPara commentsOnly
go False = foldr ($) fmtPara standard
