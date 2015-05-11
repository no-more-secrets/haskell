--------------------------------------------------------------------------------
-- Say an integer in English form
--------------------------------------------------------------------------------

module Saynumber (sayNumber) where

import Data.List (genericIndex)
import Control.Monad (guard)
import Utils

--------------------------------------------------------------------------------
-- Create list of written numbers < 1000
--------------------------------------------------------------------------------

es = enumerate · splitC

oneTo19  = es ",one,two,three,four,five,six,seven,eight,nine,ten,eleven,twelve,thirteen,fourteen,fifteen,sixteen,seventeen,eighteen,nineteen"
hundreds = es ",one hundred,two hundred,three hundred,four hundred,five hundred,six hundred,seven hundred,eight hundred,nine hundred"
tens     = es ",twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety"

oneTo999 = do
    (x',x) <- hundreds
    (y',y) <- tens
    (z',z) <- oneTo19
    guard (y'==0 || z'<10)
    return (joinStrip [x,y,z])

--------------------------------------------------------------------------------
-- Create infinite list of ranks
--------------------------------------------------------------------------------

ranks = ranks' ++ (tail ranks) `zipSpace` (repeat · last $ ranks')
    where ranks' = splitC ",thousand,million,billion,trillion,quadrillion"

--------------------------------------------------------------------------------
-- Say any positive number using the above
--------------------------------------------------------------------------------

sayPositive :: (Integral a) => a -> String
sayPositive = join ", " · reverse · map joinStripPair · remove (null · fst) · flip zip ranks · map (genericIndex oneTo999) · triples
    -- Take an integer and return a list of its triples of digits
    where triples = map (flip mod 1000) · takeWhile (>0) · iterate (`div` 1000)

--------------------------------------------------------------------------------
-- Add support for zero and negative
--------------------------------------------------------------------------------

sayNumber :: (Integral a) => a -> String
sayNumber 0 = "zero"
sayNumber n = if n > 0 then sayPositive n else "negative " ++ sayNumber (-n)
    
--------------------------------------------------------------------------------
-- Tests for sayNumber
--------------------------------------------------------------------------------

runTests = putStrLn $ if and sayNumberTests then "Success" else "Failed"

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
                                      "three hundred forty three thousand, four hundred thirty four",
                  sayNumber large2 == "one hundred forty nine billion quadrillion, one thousand quadrillion, " ++
                                      "four hundred forty four quadrillion, five billion, seven hundred eighty million",
                  sayNumber large3 == "thirty eight quadrillion, four hundred thirty eight trillion, " ++
                                      "seven hundred eighty seven billion, five"]

                  where large1 = 2323434511343434343434      :: Integer
                        large2 = 149000001444000005780000000 :: Integer
                        large3 = 38438787000000005           :: Integer
