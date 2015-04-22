module Prefix (commonURLPath, commonPrefix, unslashes, slashes) where

import Data.List (transpose)
import Utils
import Colocate

------------------------------------------------------------- 

(unslashes, slashes) = (join "/", split "/")

--commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix xs ys = map fst . takeWhile (uncurry (==)) $ zip xs ys

--commonURLPath :: [String] -> String
commonURLPath = unslashes . foldr1 commonPrefix . map slashes

------------------------------------------------------------- 
-- Tests

showTests = (unlines . colocate) matrix
    where
        matrix   = transpose tests ++ [column " = ", output, column " ?= ", baseline, map show results]
        column s = replicate (length tests) s

runTests = and results

results  = zipWith (==) output baseline
output   = map commonURLPath tests

tests = [ ["/",       "/"],
          ["",        ""],
          ["A",       "A"],
          ["/A",      "/A"],
          ["A/",      "A/"],
          ["A/B",     "A/B"],
          ["A/B/C",   "A/B/C"],
          ["A/B/X/D", "A/B/C/D"] ]

baseline = [ "/",
             "",
             "A",
             "/A",
             "A/",
             "A/B",
             "A/B/C",
             "A/B" ]

-------------------------------------------------------------

--Smallest one
--commonPrefix2 :: (Eq a) => [a] -> [a] -> [a]
--commonPrefix2 = zipWithWhile first (==) 

--Inefficient
--commonPrefix2 :: (Eq a) => [a] -> [a] -> [a]
--commonPrefix2 x y = last · takeWhile (`isPrefixOf` x) $ inits y 

--Requires no other functions
--commonPrefix2 :: (Eq a) => [a] -> [a] -> [a]
--commonPrefix2 x y = fst · unzip · takeWhile (\(x,y) -> x==y) $ zip x y

--commonPrefix2 :: (Eq a) => [a] -> [a] -> [a]
--commonPrefix2 x y = map fst $ zipWhile (==) x y

--commonPrefix2 :: (Eq a) => [a] -> [a] -> [a]
--commonPrefix2 x y = [w | (w,v) <- zipWhile (==) x y]

--commonPrefix :: (Eq a) => [[a]] -> [a]
--commonPrefix = foldr1 commonPrefix2

------------------------------------------------------------- 

--transposeCut :: [[a]] -> [[a]]
--transposeCut l = takeWhile ((length l ==) · length) $ transpose l

--commonPrefix :: (Eq a) => [[a]] -> [a]
--commonPrefix = map head · takeWhile uniform · transposeCut

------------------------------------------------------------- 

--commonURLPath2 :: [[Char]] -> [Char]
--commonURLPath2 = joinBegin "/" · commonPrefix · map (splitIgnore "/")

--commonURLPath :: [String] -> String
--commonURLPath = joinBegin "/" · foldr1 (zipWithWhile first (==)) · map (splitIgnore "/")

