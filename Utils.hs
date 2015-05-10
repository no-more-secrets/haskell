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
-- Monadic function composition
(··) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(··) = (<=<)

enumerate :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate = zip [0..]
enumerate1 :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate1 = zip [1..]

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipWhile f x y = takeWhile (uncurry f) $ zip x y

zipWithWhile :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
zipWithWhile f g x y = map (uncurry f) $ zipWhile g x y

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = (f x) && (g x)

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f = map $ \(x,y) -> (f x, y)

-- This is safe since if the list is empty the "head" function will not be called
uniform :: (Eq a) => [a] -> Bool
uniform x = all (==head x) x

groupByKey :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupByKey f xs = applyZip (f . head) groups
    where groups = groupBy ((==)`on`f) xs
--groupByKey f = mapFst head . unzip . groupBy (equating fst) . applyZip f

applyZip :: (a -> b) -> [a] -> [(b,a)]
applyZip f xs = zip (map f xs) xs

first    = const
keep     = filter
remove f = keep (not · f)
notnull :: (Foldable t) => t a -> Bool
notnull  = not · null

strip :: [Char] -> [Char]
strip = f · f 
    where f = reverse · dropWhile (==' ')
 
equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating f = (==) `on` f

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

onLines f = unlines . f . lines
onWords f = unwords . f . words

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
