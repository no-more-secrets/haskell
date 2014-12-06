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

maybeIf :: a -> Bool -> Maybe a
maybeIf x b = if b then Just x else Nothing

uniform :: (Eq a) => [a] -> Bool
uniform x = all (==head x) x

groupByKey :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByKey f = groupBy ((==) `on` f)

applyPair :: (a -> b) -> a -> (a,b)
applyPair f x = (x, f x)

first    = const
keep     = filter
remove f = keep (not · f)
notnull  = not · null

strip :: [Char] -> [Char]
strip = reverse · dropWhile (== ' ') · reverse · dropWhile (== ' ')
 
pairs xs ys = [ (x,y) | x<-xs, y<-ys ]

mapT :: (a -> b, c -> d) -> (a, c) -> (b, d)
mapT (f, g) (x, y) = (f x, g y)

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
joinStripWith c = strip · join c · map strip

joinStrip :: [[Char]] -> [Char]
joinStrip = joinStripWith " "

joinStripPair :: ([Char], [Char]) -> [Char]
joinStripPair (x, y) = joinStrip [x,y]

joinSpace :: [[Char]] -> [Char]
joinSpace = unwords · words · unwords

-- Comparison functions

compareWith :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f x y = compare (f x) (f y)
