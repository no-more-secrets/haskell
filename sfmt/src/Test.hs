-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Tests
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{-# LANGUAGE TemplateHaskell #-}

module Test (runTests) where

import Test.QuickCheck
import Hyphen
import Wrap
import Utils

import qualified Debug.Trace as T (trace)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--                              Types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newtype Columns = Columns { getColumns :: Int }
  deriving (Show)

newtype TestWord = TestWord { getWord :: String }
  deriving (Show)

newtype TestText = TestText { getString :: String }
  deriving (Show)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--                           Utilities
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trace :: (Show a) => String -> a -> a
trace s x = T.trace (s ++ ": " ++ show x) x

-- We need this because the one used inside the Hyphen module  is
-- not exposed.
isFragment :: String -> Bool
isFragment x
    | dehyphenate [x,"_"] == [x,"_"]       = False
    | dehyphenate [x,"_"] == [init x++"_"] = True
    | otherwise = error "invalid result in isFragment"

--  Allow random words consisting only of dashes
--
--  Laziness tests?
--
--  Wrap.hs Tests
--
--    * wrap
--
--        - Need test cases that supply hyphenated input to wrap
--
--  Hyphen.hs Tests
--
--    * dehyphenate
--
--        - dehyphenate, after being applied to a list, should
--          yield a result such that no item is a fragment
--          except possibly the last one.
--
--        - dehyphenate should be idempotent
--
--        - dehyphenate should never increase the number of words
--
--        - dehyphenate, when acting on a list of one word,
--          should always leave it unchanged.
--
--        - dehyphenate, when acting on a list of two words,
--          should either join them or leave them unchanged.
--
--        - dehyphenate (dehyphenate xs ++ dehyphenate ys) ==
--        - dehyphenate (xs++ys)
--
--        - dehyphenate, when applied to xs and ys, yielding
--          xs' and ys', should, when applied to (xs'++ys')
--          produce a result whose length is either equal to
--          length xs' + length ys', or one less.
--
--    * both
--
--        - hyphenate and dehyphenate should be inverses when input
--          contains no fragments (apart from possibly last word).

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--                             Hyphen
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Hyphenating an empty list of words should yield an empty list.
prop_hyphEmpty :: Bool
prop_hyphEmpty = (length (hyphenate []) == 0)

-- Hyphenating a single word should yield  a  number  of  results
-- less or equal to the number of characters in the word.
prop_hyphWordMax :: TestWord -> Property
prop_hyphWordMax (TestWord w) = len > 0 ==> (length hyph <= len)
  where
    len  = length w
    hyph = hyphenate [w]

-- If we hyphenate a  word  that  is  not  a fragment, the result
-- should consist of all fragments except the last.
prop_hyphFrags :: TestWord -> Property
prop_hyphFrags (TestWord w) = (isFragment w == False) ==> result
  where
    nonFrags = dropWhile isFragment $ hyphenate $ [w]
    result   = (length nonFrags == 1)

-- Hyphenating a word, then repeatedly hyphenating the components
-- recursively should eventually converge to be idempotent  in  a
-- number of steps less than the length of the word.
prop_hyphConverge :: TestWord -> Bool
prop_hyphConverge (TestWord w) = (first == second)
  where
    hyphs = iterate hyphenate $ [w]
    (first, second) = (hyphs !! length w, hyphs !! (length w+1))

-- Hyphenating multiple words separately  and  concatenat the re-
-- sults  it should yield the same result as hyphenating them to-
-- gether.
prop_hyphCompose :: TestText -> Bool
prop_hyphCompose (TestText s) = (composed == separate)
  where
    separate = concatMap (hyphenate . pure) $ words $ s
    composed = hyphenate $ words $ s

-- If we hyphenate a text, then the concatenated result, with all
-- dashes removed, should equal the original text with all dashes
-- removed.
prop_hyphDashDel :: TestText -> Bool
prop_hyphDashDel (TestText s) = result
  where
    noDash     = filter (/='-') $ concat $             words $ s
    hyphNoDash = filter (/='-') $ concat $ hyphenate $ words $ s
    result     = (noDash == hyphNoDash)

-- Hyphenate should never decrease the number of words
prop_hyphNoDec :: TestText -> Bool
prop_hyphNoDec (TestText s) = (length hyph >= length ws)
  where
    ws   = words s
    hyph = hyphenate ws

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--                              Wrap
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- For non-empty input string but zero columns we should  end  up
-- with precisely one word per line.
prop_wrapZeroColumns :: TestText -> Property
prop_wrapZeroColumns (TestText s) = (notNull s ==> (lengths == [1]))
  where lengths = nubNlnN $ map length $ wrap 0 $ words $ s

-- If we word wrap nothing then we should get nothing.
prop_wrapEmptyString :: Columns -> Bool
prop_wrapEmptyString (Columns n) = (wrap n [] == [])

-- If column number is  large  enough  to  hold  the entire input
-- string (and if input  is  not  empty)  then  the  word-wrapped
-- output should only have a single line.
prop_wrapSingleLine :: Columns -> Columns -> TestText -> Property
prop_wrapSingleLine (Columns n) (Columns m) (TestText s) = (fits ==> good)
  where
    s'      = take m $ unwords $ words $ s
    fits    = length s' > 0 && length s' <= n
    wrapped = wrap n $ words $ s'
    good    = (length wrapped == 1)

-- In the case that all words have length <= n then it should  be
-- the  case  that the result of word wrapping produces either no
-- lines (empty input) or produces lines that have length l which
-- is 0 < l <=  n.  In  this  case  the "length" means the length
-- after unwords'ing.
prop_wrapBounds :: Columns -> TestText -> Property
prop_wrapBounds (Columns n) (TestText s) =
    (notNull s && not tooLong ==> fits)
  where
    ws        = words s
    tooLong   = any ((>n) . length) ws
    lengths   = map length $ map unwords $ wrap n $ ws
    fits      = (maximum lengths <= n) &&
                (minimum lengths >  0)

-- For a given number of columns n it must be the case  that  any
-- resulting line which is longer than  n must have only one word
-- in it.
prop_wrapLongLine :: Columns -> TestText -> Property
prop_wrapLongLine (Columns n) (TestText s) = (notNull s ==> result)
  where
    lengths  = nubNlnN    $ map length $ keep longLine
             $ wrap n $ words      $ s
    longLine = (>n) . length . unwords
    result   = (lengths == [] || lengths == [1])

-- If we wrap a text then  the  total  number of words cannot de-
-- crease.
prop_wrapNonDecrease :: Columns -> TestText -> Bool
prop_wrapNonDecrease (Columns n) (TestText s) = (end >= start)
  where
    ws    = words s
    start = length ws
    end   = length $ concat $ wrap n $ ws

-- If we do a word wrap, then join the lines onto a single  line,
-- then dehyphenate, we should get a list of words which is iden-
-- tical to calling `words` on  the  original  input  string.  In
-- other words, if we do a word wrap and then undo the word wrap,
-- then we should end up exactly where we started.
prop_wrapInverseEqual :: Columns -> TestText -> Bool
prop_wrapInverseEqual (Columns n) (TestText s) = (end == start)
  where
    start = words s
    end   = (dehyphenate . concat . wrap n) start

-- If  we  do  a  word wrap, then join the resulting lines onto a
-- single line without dehyphenating, then apply the word wrap to
-- this list, we should obtain the same result as the first  word
-- wrap.
prop_wrapIdempotent :: Columns -> TestText -> Bool
prop_wrapIdempotent (Columns n) (TestText s) = (end == start)
  where
    start = wrap n $ words  $ s
    end   = wrap n $ concat $ start

-- The word wrap yields  a  list  of  lines;  none of these lines
-- should be empty.
prop_wrapNoEmptyLines :: Columns -> TestText -> Bool
prop_wrapNoEmptyLines (Columns n) (TestText s) = (null empty)
  where empty = keep null $ wrap n $ words $ s

-- The word wrap yields a list  of  list  of words; none of these
-- words should be empty.
prop_wrapNoEmptyWords :: Columns -> TestText -> Bool
prop_wrapNoEmptyWords (Columns n) (TestText s) = (null empty)
  where empty = keep null $ concat $ wrap n $ words $ s

-- In a wrapped text, any word which has isFragment==True must be
-- the last word on a line.
prop_wrapFragLast :: Columns -> TestText -> Bool
prop_wrapFragLast (Columns n) (TestText s) = good
  where
    fragLast = (<=1) . length . dropWhile (not . isFragment)
    good     = all fragLast $ wrap n $ words $ s

-- Given an initial  text  containing  at  least  one hyphen, the
-- total  number of hyphens in the text should be preserved after
-- wrapping and dehyphenating.
prop_wrapConstHyphs :: Columns -> TestText -> Property
prop_wrapConstHyphs (Columns n) (TestText s) = (hasHyphs ==> result)
  where
    s' = concat $ dehyphenate $ concat $ wrap n $ words $ s
    numHyphs = length . filter ('-'==)
    hasHyphs = numHyphs s > 0
    result   = (numHyphs s == numHyphs s')

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
prop_wrapGreedyNoHyph :: Columns -> TestText -> Bool
prop_wrapGreedyNoHyph (Columns n) (TestText s) = result
  where
    wrapped = wrap n $ words $ s
    result  = and $ do
      (ws,(x:_)) <- zip wrapped (drop 1 wrapped)
      let endsWithHyphen = ('-'==) $ last $ last $ ws
      let remaining = n - length (unwords ws)
      -- plus 1 for the space needed if we were to insert
      -- this word on the previous line
      return $ endsWithHyphen || (remaining < (length x + 1))

-- This  is essentially prop_wrapGreedyNoHyph but is a stronger check
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
prop_wrapGreedy :: Columns -> TestText -> Bool
prop_wrapGreedy (Columns n) (TestText s) = result
  where
    wrapped = wrap n $ words $ s
    result  = and $ do
      (ws,(x:_)) <- zip wrapped (drop 1 wrapped)
      let endsWithHyphen = ('-'==) $ last $ last $ ws
      -- Get the length of the first component of the hyphenation
      -- of  x;  note that this includes the length of the hyphen
      -- if there is one.
      let firstHyphLen = length $ head $ hyphenate . pure $ x
      let remaining    = n - length (unwords ws)
      -- plus  1  for  the space needed if we were to insert this
      -- component on the previous line
      return $ endsWithHyphen || (remaining < (firstHyphLen + 1))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

return []
runTests = $quickCheckAll
--runTests = $verboseCheckAll
--runTests = verboseCheck prop_wrapConstHyphs

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--                           Generators
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
genTextChar :: Gen Char
genTextChar = elements $ concat $
    replicate 6 ['a'..'z'] ++ ["0123456789-"]

genTextPunct :: Gen Char
genTextPunct = elements "!.?"

genTextWord :: Gen String
genTextWord = do
    len <- choose (0,20)
    infiniteWord <- infiniteListOf genTextChar
    let word = (dropEndHyphens . take len) infiniteWord
    extra <- genTextChar
    let word' = if null word then [extra] else word
    return (take len word')
  where
    dropEndHyphens :: String -> String
    dropEndHyphens = reverse . dropWhile ('-'==) . reverse

instance Arbitrary TestWord where
    arbitrary = TestWord <$> genTextWord

genTextSentence :: Gen [String]
genTextSentence = do
    words <- listOf genTextWord
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
