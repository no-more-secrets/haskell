module Utils where

import Data.List (intercalate, groupBy, genericIndex, isPrefixOf, unfoldr, findIndex, tails)
import Control.Applicative (Applicative, liftA2)
import Control.Monad ((<=<))
import Data.Function (on)

-------------------------------------------------------------
--Utility Functions
-------------------------------------------------------------

--Reading and Writing

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads (strip s) of
                  [(x, "")] -> Just x
                  _         -> Nothing

readIntMay s     = readMaybe s :: Maybe Int
readIntegerMay s = readMaybe s :: Maybe Integer

readInteger = read :: (String -> Integer)

interactLines :: (String -> String) -> IO ()
interactLines f = interact (f `byLine`)

genIdxMay :: (Integral a) => [b] -> a -> Maybe b
genIdxMay [] _     = Nothing
genIdxMay (x:_)  0 = Just x
genIdxMay (x:xs) i = if i >= 0 then genIdxMay xs (i-1) else Nothing

--In insert mode type CTRL-V 183 to get the · symbol

-- Standard function composition (C-V 183)
(·) :: (b -> c) -> (a -> b) -> a -> c
(·) = (.)
-- Applicative function composition
--() :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
--() = liftA2 (.)
-- Monadic function composition (C-Vu2025)
(‥) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(‥) = (<=<)
-- ┅ (C-Vu2505)
enumerate :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate = zip [0..]
enumerate1 :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate1 = zip [1..]

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipWhile f x y = takeWhile (uncurry f) $ zip x y

zipWithWhile :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
zipWithWhile f g x y = map (uncurry f) $ zipWhile g x y

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = (f x) && (g x)

uniform :: (Eq a) => [a] -> Bool
uniform x = all (==head x) x

groupByKey :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByKey f = groupBy ((==) `on` f)

first    = const
keep     = filter
remove f = keep (not · f)
notnull  = not · null

strip :: [Char] -> [Char]
strip = f · f 
    where f = reverse · dropWhile (==' ')
 
-- Basic split function

split :: (Eq a) => [a] -> [a] -> [[a]]
split cs xs
    | (res == Nothing) = [xs]
    | otherwise        = [take idx xs] ++ split cs (drop (idx + length cs) xs)
    where
        res      = findIndex (cs `isPrefixOf`) (tails xs)
        Just idx = res

-- Splitting and Joining

joinBegin :: [a] -> [[a]] -> [a]
joinBegin x = (x ++) · intercalate x

zipJoin :: [a] -> [[a]] -> [[a]] -> [[a]]
zipJoin elem = zipWith (\x y -> join elem [x,y])

zipSpace :: [String] -> [String] -> [String]
zipSpace = zipWith $ joinTwo " "

splitIgnore :: Eq a => [a] -> [a] -> [[a]]
splitIgnore x = remove null · split x

splitC :: [Char] -> [[Char]]
splitC = split ","

join :: [a] -> [[a]] -> [a]
join = intercalate

joinPair :: [a] -> ([a],[a]) -> [a]
joinPair c (x,y) = join c [x,y]

joinTwo :: [a] -> [a] -> [a] -> [a]
joinTwo c = curry $ joinPair c

joinStripWith :: [Char] -> [[Char]] -> [Char]
joinStripWith c = strip · join c · remove null · map strip

joinStrip :: [[Char]] -> [Char]
joinStrip = joinStripWith " "

joinStripPair :: ([Char], [Char]) -> [Char]
joinStripPair (x, y) = joinStrip [x,y]

-- Comparison functions

compareWith :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f x y = compare (f x) (f y)

onReverse f = reverse · f · reverse
byLine f    = unlines · map f · lines
