module Ranges (makeRanges, expandRanges, showRanges, readRanges, simplifyRanges) where

import GHC.Exts (sortWith)
import Utils

-------------------------------------------------------------
-- Interface

makeRanges     :: (Num a, Ord a)         => [a]     -> [(a,a)]
makeRanges      = rangesUnion · fmap (\x -> (x,x))

expandRanges   :: (Enum a, Num a, Ord a) => [(a,a)] -> [a]
expandRanges    = concat · fmap (uncurry enumFromTo) · rangesUnion

showRanges     :: (Show a, Eq a)         => [(a,a)] -> String
showRanges      = unwords · fmap (\(x,y) -> show x ++ (if x == y then "" else "-" ++ show y))

readRanges     :: (Read a)               => String  -> [(a,a)]
readRanges      = fmap (\x -> (head x, last x)) · fmap (fmap read) · fmap (split "-") · splitIgnore " "

simplifyRanges :: String -> String
simplifyRanges  = showRanges · rangesUnion · readRanges

-------------------------------------------------------------
-- The meat

rangesUnion :: (Num a, Ord a) => [(a,a)] -> [(a,a)]
rangesUnion = merge · sortWith fst · fmap sortPair
    where merge (x:y:xs) = if overlap x y then merge ((union x y):xs) else x:(merge (y:xs))
          merge x = x

-------------------------------------------------------------
-- Utilities

overlap              :: (Num a, Ord a) => (a,a) -> (a,a) -> Bool
overlap  (x,y) (u,v)  = (y >= u-1) && (x <= v+1)

union                :: (Ord a) => (a,a) -> (a,a) -> (a,a)
union    (x,y) (u,v)  = (min x u, max y v)

sortPair             :: (Ord a) => (a,a) -> (a,a)
sortPair (x,y)        = if x < y then (x,y) else (y,x)

