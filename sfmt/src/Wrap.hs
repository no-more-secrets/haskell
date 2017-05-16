-- ──────────────────────────────────────────────────────────────
--                 Word wrapping with hyphenation
-- ──────────────────────────────────────────────────────────────
module Wrap (wrapPara) where

import Data.List (inits, splitAt)
import Hyphen    (dehyphen, hyphens)
import Utils     (unfoldrList)

-- Word wrap a list of words to  fit  on  a line of size n (which
-- includes spaces). Words may be hyphenated  to  attain  optimal
-- packing. There will be at  least  one word (or hyphenated word
-- fragment) per line; if a  word  or  fragment  is longer than a
-- line then it will go on  its  own  line and the length of that
-- line will be > n.
wrapPara :: Int -> [String] -> [[String]]
wrapPara n = map dehyphen.unfoldrList one.(hyphens =<<).dehyphen
  where one  = splitAt =<< max 1.length.takeWhile fits.tail.inits
        fits = (<=n) . length . unwords . dehyphen
