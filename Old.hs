module Old where

import Data.Tuple (swap)
import Data.List (genericIndex)
--import Data.Function

import Utils

--------------------------------------------------------------------------------
-- Say an integer in written form

sayNumber :: (Integral a) => a -> String
sayNumber n
    | (n == 0) = "zero"
    | (n <  0) = "negative " ++ sayNumber (-n)
    | (n >  0) = sayPositive n

        where sayPositive = join ", " · reverse · map joinPairStrip · remove (null · fst) · (`zip` ranks) · triples

              --ranks :: [String]
              ranks = ranks' ++ (tail ranks) `zipSpace` (repeat · last $ ranks')
                  where ranks' = ["","thousand","million","billion","trillion","quadrillion"]

              --triples :: (Integral a) => a -> [String]
              triples 0 = []
              triples n = (oneTo999 `genericIndex` (n `mod` 1000)):(triples $ n `div` 1000)
                  where oneTo999 = oneTo99 ++          [joinStripTwo x y | x <- hundreds, y <- oneTo99]
                        oneTo99  = oneTo9  ++ teens ++ [joinStripTwo x y | x <- tens,     y <- oneTo9]
                        oneTo9   = ["","one","two","three","four","five","six","seven","eight","nine"]
                        teens    = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
                        tens     = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
                        hundreds = ["one hundred","two hundred","three hundred","four hundred","five hundred","six hundred","seven hundred","eight hundred","nine hundred"]

--------------------------------------------------------------------------------
-- Tests for sayNumber

sayNumberTest  = and sayNumberTests
sayNumberTests = [sayNumber 0      == "zero",
                  sayNumber 1      == "one",
                  sayNumber 10     == "ten",
                  sayNumber 13     == "thirteen",
                  sayNumber 85     == "eighty five",
                  sayNumber 100    == "one hundred",
                  sayNumber 101    == "one hundred one",
                  sayNumber 1000   == "one thousand",
                  sayNumber 1017   == "one thousand, seventeen",
                  sayNumber 1234   == "one thousand, two hundred thirty four",
                  sayNumber (-4)   == "negative four",
                  sayNumber large1 == "two million quadrillion, three hundred twenty three thousand quadrillion, " ++
                                      "four hundred thirty four quadrillion, five hundred eleven trillion, "       ++
                                      "three hundred forty three billion, four hundred thirty four million, "      ++
                                      "three hundred forty three thousand, four hundred thirty four"]

                  where large1 = 2323434511343434343434 :: Integer

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
 
