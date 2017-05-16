-- ──────────────────────────────────────────────────────────────
-- Tests
-- ──────────────────────────────────────────────────────────────

{-# LANGUAGE TemplateHaskell #-}

module Test (runTests) where

import Hyphen
import Test.QuickCheck
import Utils
import Wrap

import qualified Debug.Trace as T (trace)

-- ──────────────────────────────────────────────────────────────
-- Types for random generation
-- ──────────────────────────────────────────────────────────────

newtype Columns = Columns { getColumns :: Int }
  deriving (Show)

newtype TestText = TestText { getString :: String }
  deriving (Show)

-- ──────────────────────────────────────────────────────────────
-- Utilities
-- ──────────────────────────────────────────────────────────────

trace :: (Show a) => String -> a -> a
trace s x = T.trace (s ++ ": " ++ show x) x

-- ──────────────────────────────────────────────────────────────
-- Tests for wrapPara
-- ──────────────────────────────────────────────────────────────

-- For non-empty input string but zero columns we should  end  up
-- with precisely one word per line.
prop_zeroColumns :: TestText -> Property
prop_zeroColumns (TestText s) = (notNull s ==> (lengths == [1]))
  where lengths = nubNlnN $ map length $ wrapPara 0 $ words $ s

-- If we word wrap nothing then we should get nothing.
prop_emptyString :: Columns -> Bool
prop_emptyString (Columns n) = (wrapPara n [] == [])

-- If column number is  large  enough  to  hold  the entire input
-- string then the word-wrapped output  should only have a single
-- line (or no lines if input is empty).
prop_singleLine :: TestText -> Property
prop_singleLine (TestText s) = (notNull s ==> (length wrapped == 1))
  where
    ws      = words s
    columns = length $ unwords $ ws
    wrapped = wrapPara columns $ ws

-- In the case that all words have length <= n then it should  be
-- the  case  that the result of word wrapping produces either no
-- lines (empty input) or produces lines that have length l which
-- is 0 < l <=  n.  In  this  case  the "length" means the length
-- after unwords'ing.
prop_bounds :: Columns -> TestText -> Property
prop_bounds (Columns n) (TestText s) =
    (notNull s && not tooLong ==> fits)
  where
    ws        = words s
    tooLong   = any ((>n) . length) ws
    lengths   = map length $ map unwords $ wrapPara n $ ws
    fits      = (maximum lengths <= n) &&
                (minimum lengths >  0)

-- For a given number of columns n it must be the case  that  any
-- resulting line which is longer than  n must have only one word
-- in it.
prop_longLine :: Columns -> TestText -> Property
prop_longLine (Columns n) (TestText s) = (notNull s ==> result)
  where
    lengths  = nubNlnN    $ map length $ keep longLine
             $ wrapPara n $ words      $ s
    longLine = (>n) . length . unwords
    result   = (lengths == [] || lengths == [1])

-- If we do a word wrap, then join the lines onto a single  line,
-- then dehyphenate, we should get a list of words which is iden-
-- tical to calling `words` on  the  original  input  string.  In
-- other words, if we do a word wrap and then undo the word wrap,
-- then we should end up exactly where we started.
prop_inverseEqual :: Columns -> TestText -> Bool
prop_inverseEqual (Columns n) (TestText s) = (end == start)
  where
    start = words s
    end   = (dehyphen . concat . wrapPara n) start

-- This is essentiall prop_inverseEqual except that we do not de-
-- hyphenate. In other words, if we do a word wrap, then join the
-- resulting lines onto a single line , then apply the word  wrap
-- to  this  list,  we should obtain the same result as the first
-- word wrap.
prop_idempotent :: Columns -> TestText -> Bool
prop_idempotent (Columns n) (TestText s) = (end == start)
  where
    start = wrapPara n $ words  $ s
    end   = wrapPara n $ concat $ start

-- The word wrap yields a list  of  list  of words; none of these
-- words should be empty.
prop_noEmptyWords :: Columns -> TestText -> Bool
prop_noEmptyWords (Columns n) (TestText s) = (null empty)
  where empty = keep null $ concat $ wrapPara n $ words $ s

-- If  a  word  wrap  output gives rise to multiple lines then it
-- must  be the case the the amount of free space at the end of a
-- given line must be shorter than  the  length of the first word
-- on the following line (otherwise  that  first word on the fol-
-- lowing line should have been  included  in the previous line).
-- Note that this does not attempt to hyphenate the first word on
-- the  next  line,  so it is a weaker check in that regard. Fur-
-- thermore,  if  the last word on the line is hyphenated then it
-- will short-circuit and pass that line since, due to the nonre-
-- cursively symmetric nature of the hyphenation algorithm  being
-- used here, it is possible that the  next word on the next line
-- will be hyphenated to a small-enough  size that it could actu-
-- ally have fit  on  the  previous  line.  That  said, this test
-- should  still  fine  blatant problems like if the word wrapper
-- makes lines that that would be too short even with hyphenation
-- turned off.
prop_greedyNoHyph :: Columns -> TestText -> Bool
prop_greedyNoHyph (Columns n) (TestText s) = result
  where
    wrapped = wrapPara n $ words $ s
    result  = and $ do
      (ws,(x:_)) <- zip wrapped (drop 1 wrapped)
      let endsWithHyphen = ('-'==) $ last $ last $ ws
      let remaining = n - length (unwords ws)
      -- plus 1 for the space needed if we were to insert
      -- this word on the previous line
      return $ endsWithHyphen || (remaining < (length x + 1))

-- This  is essentially prop_greedyNoHyph but is a stronger check
-- which  also tries to hyphenate the first word on the next line
-- to try to fit it on the previous line (and if  that  fit  suc-
-- ceeds then it's a failure). However, it will short-circuit and
-- return true for a line if that line  ends  with  a  hyphenated
-- word. This is because in that case  we  cannot  hyphenate  the
-- first word on the next line because it is part of the  hyphen-
-- ation and the hyphenation algorithm  being  used does not have
-- the property that  hyphenating  the  suffix  of  a hyphenation
-- yields the same hyphenation  structure  as  when we hyphenated
-- the original word.
prop_greedy :: Columns -> TestText -> Bool
prop_greedy (Columns n) (TestText s) = result
  where
    wrapped = wrapPara n $ words $ s
    result  = and $ do
      (ws,(x:_)) <- zip wrapped (drop 1 wrapped)
      let endsWithHyphen = ('-'==) $ last $ last $ ws
      -- Get the length of the first component of the hyphenation
      -- of  x;  note that this includes the length of the hyphen
      -- if there is one.
      let firstHyphLen = length $ head $ hyphens $ x
      let remaining    = n - length (unwords ws)
      -- plus  1  for  the space needed if we were to insert this
      -- component on the previous line
      return $ endsWithHyphen || (remaining < (firstHyphLen + 1))

return []
runTests = $quickCheckAll
--runTests = $verboseCheckAll
--runTests = verboseCheck prop_idempotent

-- ──────────────────────────────────────────────────────────────

genTextChar :: Gen Char
genTextChar = elements $ concat $
    replicate 6 ['a'..'z'] ++ ["0123456789-"]

genTextPunct :: Gen Char
genTextPunct = elements "!.?"

genTextWord :: Gen String
genTextWord = do
    len <- choose (0,15) 
    infiniteWord <- infiniteListOf genTextChar
    let word = (dropEdgeHyphens . take len) infiniteWord
    extra <- genTextChar
    let word' = if null word then [extra] else word'
    return (take len word)
  where
    dropEdgeHyphens :: String -> String
    dropEdgeHyphens = reverse . dropWhile ('-'==)
                    . reverse . dropWhile ('-'==)

genTextSentence :: Gen [String]
genTextSentence = do
    words <- listOf1 genTextWord
    lastWord <- genTextWord
    punct <- genTextPunct
    return $ words ++ [lastWord ++ [punct]]

instance Arbitrary TestText where
    arbitrary = (TestText . unwords) <$> genTextPara

genTextPara :: Gen [String]
genTextPara = concat <$> listOf genTextSentence

genColumns :: Gen Int
genColumns = choose (0,80)

instance Arbitrary Columns where
    arbitrary = Columns <$> genColumns
