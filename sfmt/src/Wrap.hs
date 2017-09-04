module Wrap (wrap) where

import Data.List       (splitAt, inits)
import Data.List.Split (chop)
import Hyphen          (hyphenate, dehyphenate)

(h, d) = (hyphenate, dehyphenate)

-- Word wrap a list of words to  fit  on  a line of size n (which
-- includes spaces). Words may be hyphenated  to  attain  optimal
-- packing. There will be at  least  one word (or hyphenated word
-- fragment) per line; if a  word  or  fragment  is longer than a
-- line then it will go on  its  own  line and the length of that
-- line will be > n.
wrap :: Int -> [String] -> [[String]]
wrap n = map d . chop (splitAt =<< max 1 . length . fit) . h . d
  where fit = takeWhile (<=n).map (length.unwords.d).tail.inits
