module SmartFormat (fmt) where

import Utils (commonPrefixAll, unfoldrList, chunks)

-- just like map, but apply f only to elements after the first.
mapTail :: (a -> a) -> [a] -> [a]
mapTail _ []     = []
mapTail f (x:xs) = x:map f xs

-- Take number of columns and input string representing a
-- document and reformats the text so that it fits within
-- the specified number of columns.
para :: Int -> [String] -> [[String]]
para n = unfoldrList (splitAt =<< longestFit)
  where
    -- Max number of words that can fit in width n (possibly
    -- none).  By "fit" it is meant to include the spaces that
    -- would be inserted between words if they were to be joined.
    -- If not even the first word fits then the function will
    -- return one since the word must go on a line.
    longestFit :: [String] -> Int
    longestFit = max 1 . length . takeWhile (<=n) . scanl1 (+) .
                 mapTail (+1) . map length

-- An unwords function with user-defined number of spaces
-- between words.
unwordsN :: Int -> [String] -> String
unwordsN n = concat . mapTail (replicate n ' ' ++)

-- Division that rounds to positive infinity.  This works
-- because `div` rounds to negative infinity.
divPI :: (Integral a) => a -> a -> a
x`divPI`y = -((-x)`div`y)

justify :: Int -> [String] -> String
justify width xs | slots <= 0 = single
                 | need  <  0 = single
                 | base  >  5 = single
                 | need' == 0 = short xs
                 | otherwise  = inflated xs
  where
    inflated :: [String] -> String
    inflated = long . map short . chunks (slots`divPI`need')

    single = unwords xs
    need   = width - length single
    slots  = length xs - 1

    (base, need') = (need`div`slots, need`mod`slots)

    short, long :: [String] -> String
    (short, long) = (unwordsN $ base+1, unwordsN $ base+2)

fmt :: Int -> String -> String
fmt n = unlines . map (justify n) . para n . words
