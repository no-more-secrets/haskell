module Old where

import Data.Tuple (swap)
import Utils

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
