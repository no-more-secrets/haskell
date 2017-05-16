module Hyphen ( hyphens
              , dehyphen
              ) where

import Text.Hyphenation (hyphenate, english_US)
import Utils            (endsWith, startsWith)

-- We can't do this with a  simple  map operation because we need
-- to join adjacent words after removing a hyphen.
dehyphen :: [String] -> [String]
dehyphen (x:y:rest)
    | x`startsWith`"-" = x:dehyphen (y:rest)
    | x`endsWith`"-"   = dehyphen $ (init x++y):rest
    | otherwise        = x:dehyphen (y:rest)
dehyphen xs         = xs

hyphens :: String -> [String]
hyphens s = (map (++"-") $ init $ chunks) ++ [last chunks]
  where chunks = hyphenate english_US s
