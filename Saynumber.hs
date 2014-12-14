module Saynumber (sayNumber) where

import Data.List (genericIndex)
import Control.Monad (guard)
import Utils

--------------------------------------------------------------------------------
-- Say an integer in English form

oneTo19  = (enumerate · splitC) ",one,two,three,four,five,six,seven,eight,nine,ten,eleven,twelve,thirteen,fourteen,fifteen,sixteen,seventeen,eighteen,nineteen"
hundreds = (enumerate · splitC) ",one hundred,two hundred,three hundred,four hundred,five hundred,six hundred,seven hundred,eight hundred,nine hundred"
tens     = (enumerate · splitC) ",twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety"
ranks    = splitC ",thousand,million,billion,trillion,quadrillion"

sayNumber :: (Integral a) => a -> String
sayNumber n
    | (n == 0)  = "zero"
    | (n <  0)  = "negative " ++ sayNumber (-n)
    | otherwise = join ", " · reverse · map joinStripPair · remove (null · fst) · flip zip ranks' · map (genericIndex oneTo999 · flip mod 1000) · takeWhile (>0) · iterate (`div` 1000) $ n

        where oneTo999 = [ joinStrip [x,y,z] | (x',x) <- hundreds, (y',y) <- tens, (z',z) <- oneTo19, (y'==0 || z'<10) ]
              ranks'   = ranks ++ (tail ranks') `zipSpace` (repeat · last $ ranks)

--------------------------------------------------------------------------------
-- Tests for sayNumber

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
