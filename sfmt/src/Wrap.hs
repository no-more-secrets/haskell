module Wrap (wrap, hyphenate, dehyphenate) where

import Data.List (splitAt, transpose)
import Hyphen    (hyphenate, dehyphenate, isHyp)
import Utils     (unfoldrList)

-- ["do-","cu-","ment-","ation","do-","cu-","ment-","ation"]
-- [    1,    1,      1,      0,    1,    1,      1,      0] -- has dash
-- [    2,    2,      4,      5,    2,    2,      4,      5] -- lengths no dash
-- [    0,    0,      0,      1,    0,    0,      0,      1] -- has no dash
-- [    0,    0,      0,      0,    1,    0,      0,      0] -- shifted
-- [    2,    2,      4,      5,    3,    2,      4,      5] -- lengths no dash shifted
-- [    2,    4,      8,     13,   16,   18,     22,     27] -- accum lengths no dash
-- [    3,    5,      9,     13,   17,   19,     23,     27] -- add
longest :: Int -> [String] -> Int
longest n xs = length . takeWhile (<=n)
             $ zipWith (+) hyps . scanl1 (+) . map sum . transpose
             $ [map length xs, 0:map (1-) hyps, map negate hyps]
  where
    hyps = map isHyp xs

-- Word wrap a list of words to  fit  on  a line of size n (which
-- includes spaces). Words may be hyphenated  to  attain  optimal
-- packing. There will be at  least  one word (or hyphenated word
-- fragment) per line; if a  word  or  fragment  is longer than a
-- line then it will go on  its  own  line and the length of that
-- line will be > n.
wrap :: Int -> [String] -> [[String]]
wrap n = map dehyphenate . wrap' . (hyphenate =<<) . dehyphenate
  where wrap' = unfoldrList (splitAt =<< max 1 . longest n)
