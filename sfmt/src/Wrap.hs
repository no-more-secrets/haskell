module Wrap (wrap, hyphenate, dehyphenate) where

import Data.List  (splitAt, transpose)
import Hyphen     (hyphenate, dehyphenate, isFragment)
import Utils      (unfoldrList)

x -| y = zipWith (-) x y
x +| y = zipWith (+) x y

-- Takes  a  column size and list of hyphenated fragments and re-
-- turns the maximum number of fragments that, after  dehypheniz-
-- ing, would be able to  fit  within  the column size (including
-- spaces between words).
-- xs: ["do-","cu-","ment-","ation","do-","cu-","ment-","ation"]
fit :: Int -> [String] -> Int
fit n xs = length $ takeWhile (<=n) $ (hyp +|) $ scanl1 (+) $ zs
  where
    -- length without hyphen + leading space if no hyphen before
    zs  = map length xs -| hyp +| (0:map (1-) hyp)
    hyp = map (fromEnum . isFragment) xs

-- Word wrap a list of words to  fit  on  a line of size n (which
-- includes spaces). Words may be hyphenated  to  attain  optimal
-- packing. There will be at  least  one word (or hyphenated word
-- fragment) per line; if a  word  or  fragment  is longer than a
-- line then it will go on  its  own  line and the length of that
-- line will be > n.
wrap :: Int -> [String] -> [[String]]
wrap n = map dehyphenate . wrap' . (hyphenate =<<) . dehyphenate
  where wrap' = unfoldrList (splitAt =<< max 1 . fit n)
