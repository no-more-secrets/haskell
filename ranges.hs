import Data.List (nub, sortBy, sort, group, unfoldr)
import GHC.Exts (sortWith)
import Utils

-------------------------------------------------------------

rangesUnion :: (Integral a) => [(a,a)] -> [(a,a)]
rangesUnion = rangesUnion' · sortWith fst · map order
    where
        order (x,y) = if x < y then (x,y) else (y,x)
        rangesUnion' ((x,y):(u,v):xs)
            | (u <= y+1) = rangesUnion' ((x, max y v):xs)
            | otherwise  = (x,y):(rangesUnion' ((u,v):xs))
        rangesUnion' l = l

        -- Version 2
        rangesUnion'' = unfoldr rangesGen
            where
                overlap ((x,y),(u,v)) = y >= u-1
                union   ((x,y),(u,v)) = (x, max y v)
                rangesGen x = if null x then Nothing else Just (newPair, rest)
                    where (newPair, rest) = mapT (union · last, map snd) · span overlap $ zip (scanl (curry union) (head x) x) x

-------------------------------------------------------------

makeRanges :: (Integral a) => [a] -> [(a,a)]
makeRanges = rangesUnion · map (\x -> (x,x))

expandRanges :: (Integral a) => [(a,a)] -> [a]
expandRanges = concat · map (uncurry enumFromTo) · rangesUnion

-------------------------------------------------------------

showRanges :: [(Integer,Integer)] -> String
showRanges = unwords · map (\(x,y) -> show x ++ (if x == y then "" else "-" ++ show y))

readRanges :: String -> [(Integer, Integer)]
readRanges = map (\x -> (head x, last x)) · map (map readInteger) · map (split "-") · splitIgnore " "

-------------------------------------------------------------

simplifyRanges :: String -> String
simplifyRanges = showRanges · rangesUnion · readRanges

-------------------------------------------------------------








-------------------------------------------------------------

--makeRanges :: (Num a, Ord a, Enum a) => [a] -> [(a,a)]
--makeRanges = map (\x -> (head x, last x)) · map (map snd) · groupByKey (uncurry (-)) · enumerate · map head · group · sort

