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
import Data.Maybe      (fromMaybe, isJust)
import Justify         (justify)
import Safe            (headMay)
import Utils           (commonPrefixAll, byLine, strip, fAnd
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

fmtNoOp :: FMT
fmtNoOp _ s = s

fmtDebug :: FMT
fmtDebug _ = unlines . map (\_ -> ".") . lines

-- ──────────────────────────────────────────────────────────────
--                           Formatter
-- ──────────────────────────────────────────────────────────────
-- Perform the word wrap and  justification; this function is in-
-- tended to be  called  after  any  preprocessing functions have
-- e.g. removed spaces or comment prefixes.
fmtWrap :: FMT
--fmtWrap c = unlines . map (justify n) . wrap n . words
fmtWrap c = unlines . map unwords . wrap n . words
  where n = target c

-- ──────────────────────────────────────────────────────────────
--                      Formatting wrappers
-- ──────────────────────────────────────────────────────────────
-- Function will look at the number of leading spaces on the
-- first line and record it. Then, it will strip all leading
-- spaces from all lines, apply the formatting function with re-
-- duced number of columns, then will re-attach a fixed number of
-- spaces (the amount found on the first line) to all lines, ef-
-- fectively making them line up.
fmtLeadingSpace :: FMT -> FMT
fmtLeadingSpace f c xs = noSpaces (f c') xs
  where prefix     = takeWhile (' '==) $ xs
        noSpaces f = byLine (prefix++) . f . byLine strip
        --c'         = Config (target c-length prefix) (comments c)
        c'         = c{ target = target c-length prefix }

-- Will apply the fiven formatting function, but first will see
-- if the lines in the text each begin with a known code-comment
-- prefix; if so, this prefix will be stripped off of each line
-- before applying the formatting function, and then re-applied
-- after. Also, the target column number given to the formatting
-- function is decreased by the length of the comment prefix.
fmtComments :: FMT -> FMT
fmtComments f c s = let
    commonPrefix  = (commonPrefixAll . lines) s
    commentPrefix = flip find (comments c)
                  . startsWith $ commonPrefix
    prefix'       = fromMaybe "" commentPrefix
    size          = length prefix'
    c'            = Config (target c-size) (comments c)
 in byLine (prefix'++) . f c' . byLine (drop size) $ s

-- chopOn odd [3,4,5,7,6,6,9,8,8] --> [[3,4],[5],[7,6,6],[9,8,8]]
chopOn :: (a -> Bool) -> [a] -> [[a]]
chopOn _ [] = []
chopOn f xs@(x:rest) = firstChunk:chopOn f remainder
  where
    firstChunk = x:(takeWhile (not . f) rest)
    remainder  = dropWhile (not . f) rest

-- Will format bullet points of this form:
--
--     - this is a bullet point
--       this is a bullet point
--     - this is another bullet point
--       this is another bullet point
--
-- where '-' is whatever character is passed in as the first arg.
fmtBullets :: Char -> FMT -> FMT
fmtBullets bulletChar f c s
  | shouldFormat = concat . map unlines . map addBullet
                          . map lines . map (f c') . map unlines
                          . map removeBullet $ bulletChunks
  | otherwise = (f c) s
  where
    shouldFormat = (length bulletChunks > 1) ||
                   (take 1 s == [bulletChar])

    bulletChunks = (splitOnBullets . lines) s
      where
        splitOnBullets :: [String] -> [[String]]
        splitOnBullets = chopOn hasBullet
          where hasBullet l = l`startsWith`[bulletChar, ' ']

    c' = c{ target = target c-2 }

    removeBullet :: [String] -> [String]
    removeBullet []     = []
    removeBullet (x:xs) = (removeBullet' x):map removeAtMostTwoSpaces xs
      where
        removeBullet' :: String -> String
        removeBullet' (bulletChar:' ':rest) = rest
        removeBullet' xs = xs

        removeAtMostTwoSpaces :: String -> String
        removeAtMostTwoSpaces (' ':' ':rest) = rest
        removeAtMostTwoSpaces (' ':rest) = rest
        removeAtMostTwoSpaces ys = ys

    addBullet :: [String] -> [String]
    addBullet (x:xs) = (bulletChar:' ':x):map (\s -> ' ':' ':s) xs

-- Will format number points into this form:
--
--     1. this is a number point
--        this is a number point
--     2. this is another number point
--        this is another number point
--
fmtNumbers :: FMT -> FMT
fmtNumbers f c s
  | shouldFormat = concat . map unlines . zipWith addNumber [1..]
                          . map lines . map (f c') . map unlines
                          . map removeNumber $ numberChunks
  | otherwise = (f c) s
  where
    shouldFormat = (length numberChunks > 1) &&
                   (take 2 s == ['1', '.'])

    numberChunks = (splitOnNumbers . lines) s
      where
        splitOnNumbers :: [String] -> [[String]]
        splitOnNumbers = chopOn (hasNumber`fAnd`hasLeadingDigit)
          where
            isDigit :: Char -> Bool
            isDigit = (`elem`"0123456789")

            hasLeadingDigit :: String -> Bool
            hasLeadingDigit = fromMaybe False . fmap isDigit . headMay

            hasNumber :: String -> Bool
            hasNumber = (==".") . take 1 . dropWhile isDigit

    prefixSizeNeeded :: Int
    prefixSizeNeeded = (+2) . length . show . length $ numberChunks
    c' = c{ target = target c-prefixSizeNeeded }

    removeNumber :: [String] -> [String]
    removeNumber []     = []
    removeNumber (x:xs) = (removeNumber' x):map (dropAtMostNSpaces prefixSizeNeeded) xs
      where
        removeNumber' :: String -> String
        removeNumber' = dropAtMostNSpaces 1 . drop 1 . dropWhile (/= '.')

        dropAtMostNSpaces :: Int -> String -> String
        dropAtMostNSpaces n s = (dropWhile (==' ') . take n $ s) ++ (drop n s)

    addNumber :: Int -> [String] -> [String]
    addNumber n (x:xs) = firstLine:remainingLines
      where
        firstLine = show n ++ "." ++ replicate paddingNeeded ' ' ++ x
        remainingLines = map (\s -> (replicate prefixSizeNeeded ' ') ++ s) xs

        paddingNeeded :: Int
        paddingNeeded = prefixSizeNeeded - length (show n) - 1 -- (-1 for dot)

-- Apply  the  given  formatting function to each paragraph, then
-- rejoin the paragraphs.
fmtMultiPara :: FMT -> FMT
fmtMultiPara f c = intercalate "\n" . map (f c) . map unlines
                 . splitOn [""]     . lines

-- Here we group all the lines by whether or not they start with
-- comment markers. Then, we only apply the formatting function
-- to those groups that begin with comments. This is to facilite
-- e.g. selecting all the lines in a code file and then running
-- the formatter but only to affect the lines that begin with
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
-- We need two fmtMultiPara's because we may of comments split
-- into paragraphs both outside of the common prefix or inside
-- it.
standard = [fmtMultiPara  -- inner
           ,fmtLeadingSpace
           ,fmtComments
           ,fmtMultiPara
           ,fmtLeadingSpace
           ,fmtBullets '*'
           ,fmtBullets '-'
           ,fmtNumbers
           ,fmtMultiPara] -- outter

commentsOnly = fmtCommentsOnly:standard

go :: Bool -> FMT
go True  = foldr ($) fmtWrap commentsOnly
go False = foldr ($) fmtWrap standard
