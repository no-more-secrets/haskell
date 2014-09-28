import Data.Tuple
import Data.List.Split (splitOn)
import Data.List (transpose, intercalate, isPrefixOf, inits, sort, unfoldr, nub)
import Data.Function

import Utils

showRangesf :: (Integer -> String) -> [Integer] -> String
showRangesf f = unwords · map showListEnds · map (map snd) · groupByKey (\(x,y) -> x-y) · enumerate · sort

    where showListEnds (x:[]) = f x
          showListEnds (x:xs) = f x ++ "-" ++ f (last xs)

-------------------------------------------------------------

showRangesStandard = showRangesf show
showRangesVerbose  = showRangesf $ \x -> "`" ++ sayNumber x ++ "'"

-------------------------------------------------------------

expandRanges :: String -> [Integer]
expandRanges = nub · sort · concat · map (uncurry enumFromTo) · map listEnds · map (map readInteger) · map (split "-") · splitIgnore " "

    where listEnds (x:[]) = (x,x)
          listEnds (x:xs) = (x,last xs)

-------------------------------------------------------------

tens     = ["twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety"]
oneTo9   = ["","one","two","three","four","five","six","seven","eight","nine"]
teens    = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
hundreds = ["one hundred","two hundred","three hundred","four hundred","five hundred","six hundred","seven hundred","eight hundred","nine hundred"]

sayNumber :: (Integral a) => a -> [Char]
sayNumber n
    | (n == 0) = "zero"
    | (n <  0) = "negative " ++ sayNumber (-n)
    | (n >  0) = (join ", " · reverse · map joinPairStrip · remove (null · fst) · (`zip` ranks) · map (oneTo999 `ii`) · unfoldr triples) n

          where triples x   = (x `mod` 1000, x `div` 1000) `maybeIf` (x>0)
                oneTo999    = oneTo99 ++          (pairsWith joinStripTwo hundreds oneTo99)
                oneTo99     = oneTo9  ++ teens ++ (pairsWith joinStripTwo tens     oneTo9)
                ranks       = ["","thousand","million","billion"] ++ (tail ranks) `zipSpace` (repeat "billion")

------------------------------------------------------------- 

primeNumber :: Int -> Int
primeNumber n = primeNumbers !! (n-1)

primeNumbers :: [Int]
primeNumbers = filter isPrime [1..]

isPrime :: Int -> Bool
isPrime n = filter (evenlyDivides n) [1..n] == [1,n] 

evenlyDivides :: Int -> Int -> Bool
evenlyDivides n = (0==) · mod n

-------------------------------------------------------------

bresenham :: Int -> Int -> [(Int,Int)]
bresenham x y
    | (x <  0) = map (\(r, w) -> (-r,w)) $ bresenham (-x) y
    | (y <  0) = map (\(r, w) -> (r,-w)) $ bresenham x (-y)
    | (y >  x) = map swap $ bresenham y x
    | (x == 0) = [(0,0)]
    | (y <= x) = map (\r -> (r,div (r*2*y+x) (2*x))) $ [0..x]
 
