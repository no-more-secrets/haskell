module Utils where

import Data.List (intercalate, groupBy, genericIndex, isPrefixOf, unfoldr, findIndex, tails)
import Data.Function (on)

-------------------------------------------------------------
--Utility Functions
-------------------------------------------------------------

--Reading and Writing

readInteger = read :: (String -> Integer)

--In insert mode type CTRL-V 183 to get this
(·) = (.)

ii :: (Integral a) => [b] -> a -> b
ii = genericIndex

enumerate :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate = zip [0..]

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipWhile f x y = takeWhile (uncurry f) $ zip x y

zipWithWhile :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
zipWithWhile f g x y = map (uncurry f) $ zipWhile g x y

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = (f x) && (g x)

maybeIf :: a -> Bool -> Maybe a
maybeIf x b = if b then Just x else Nothing

groupByKey :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByKey f = groupBy ((==) `on` f)

first    = const
keep     = filter
remove f = keep (not · f)
notnull  = not · null

strip :: [Char] -> [Char]
strip = reverse · dropWhile (== ' ') · reverse · dropWhile (== ' ')
 
pairs xs ys       = [ (x,y) | x<-xs, y<-ys ]
pairsWith f xs ys = [ f x y | x<-xs, y<-ys ]

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

zipSpace = zipWith (\x y -> x++" "++y)

splitIgnore :: Eq a => [a] -> [a] -> [[a]]
splitIgnore x = remove null · split x

splitComma :: [Char] -> [[Char]]
splitComma = split ","

join :: [a] -> [[a]] -> [a]
join = intercalate

joinPair :: [a] -> ([a],[a]) -> [a]
joinPair c (x,y) = join c [x,y]

joinTwo :: [a] -> [a] -> [a] -> [a]
joinTwo c = curry $ joinPair c

joinStripWith :: [Char] -> [[Char]] -> [Char]
joinStripWith c = strip · join c · map strip

joinStrip :: [[Char]] -> [Char]
joinStrip = joinStripWith " "

joinStripTwo :: [Char] -> [Char] -> [Char]
joinStripTwo x y = joinStrip [x, y]

joinPairStrip :: ([Char], [Char]) -> [Char]
joinPairStrip (x, y) = joinStripTwo x y

joinSpace :: [[Char]] -> [Char]
joinSpace = unwords · words · unwords

-- Comparison functions

compareWith :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f x y = compare (f x) (f y)
